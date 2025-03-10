#!/usr/bin/env bb

;; This script runs a small web application to let users subscribe to
;; a Mailgun mailing list.
;;
;; You will need a Mailgun API endpoint, key and the list identifier.
;;
;; Store them in environment variables:
;;
;; MAILGUN_LIST_ID (example: "my@list.com")
;; MAILGUN_API_ENDPOINT (example "https://api.eu.mailgun.net/v3")
;; MAILGUN_API_KEY (example "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx-xxxxxxxx-xxxxxxxx"
;; SUBSCRIBE_BASE_PATH (optional, example: "/app" - for subdirectory deployments)
;;
;; Running the web application as http://localhost:8080
;;
;; ~$ subscribe
;;
;; Running it on another port (e.g. http://localhost:4444)
;;
;; ~$ subscribe 4444
;;
;; Reading UI strings from a config.edn file:
;;
;; ~$ subscribe --config config.edn
;;
;; See the default value of ui-strings below to see what UI strings
;; you can configure.

(require '[org.httpkit.server :as server]
         '[babashka.http-client :as http]
         '[clojure.string :as str]
         '[cheshire.core :as json]
         '[taoensso.timbre :as log]
         '[clojure.edn :as edn])

;; Default language setting
(def default-language :en)

;; Default logging level
(def log-min-level :info)

;; Configure Timbre logging
(log/merge-config!
 {:min-level      log-min-level
  :timestamp-opts {:pattern "yyyy-MM-dd HH:mm:ss"}
  :appenders      {:println {:enabled? true}}})

;; Anti-Spam protections
(def rate-limit-window (* 60 60 1000)) ;; 1 hour in milliseconds
(def max-requests-per-window 5) ;; Maximum 5 requests per IP per hour
(def ip-request-log (atom {}))

;; Set up environment variables
(def mailgun-list-id
  (or (System/getenv "MAILGUN_LIST_ID")
      (log/error "Missing MAILGUN_LIST_ID")))
(def mailgun-api-endpoint
  (or (System/getenv "MAILGUN_API_ENDPOINT")
      (log/error "Missing MAILGUN_API_ENDPOINT")))
(def mailgun-api-key
  (or (System/getenv "MAILGUN_API_KEY")
      (log/error "Missing MAILGUN_API_KEY")))

;; Base path configuration for subdirectory deployments
(def base-path
  (let [path (or (System/getenv "SUBSCRIBE_BASE_PATH") "")]
    (if (str/blank? path)
      ""
      (if (str/ends-with? path "/")
        (str/replace path #"/$" "")  ;; Remove trailing slash
        path))))

;; Log configuration
(log/info "MAILGUN_LIST_ID:" mailgun-list-id)
(log/info "MAILGUN_API_ENDPOINT:" mailgun-api-endpoint)
(log/info "MAILGUN_API_KEY:" (if mailgun-api-key "****" "Not set"))
(log/info "SUBSCRIBE_BASE_PATH:" (if (str/blank? base-path) "[not set]" base-path))

;; Helper function to construct paths with the base path
(defn make-path [& segments]
  (let [segments (remove str/blank? segments)]
    (str base-path
         (if (and (not (str/blank? base-path))
                  (not (str/starts-with? (first segments) "/")))
           "/"
           "")
         (str/join "/" segments))))

;; Centralized Mailgun Authentication Helper
(defn get-mailgun-auth-header
  "Returns the Authorization header value for Mailgun API requests"
  []
  (let [auth-string  (str "api:" mailgun-api-key)
        auth-bytes   (.getBytes auth-string)
        encoder      (java.util.Base64/getEncoder)
        encoded-auth (.encodeToString encoder auth-bytes)]
    (str "Basic " encoded-auth)))

;; Centralized URL construction functions
(defn get-mailgun-member-url
  "Constructs the URL for a specific member"
  [email]
  (format "%s/lists/%s/members/%s"
          mailgun-api-endpoint
          mailgun-list-id
          (java.net.URLEncoder/encode email "UTF-8")))

(defn get-mailgun-members-url
  "Constructs the URL for the members list"
  []
  (format "%s/lists/%s/members"
          mailgun-api-endpoint
          mailgun-list-id))

(defn make-mailgun-request
  "Makes a request to the Mailgun API with appropriate authentication"
  [method url body-params]
  (let [auth-header  (get-mailgun-auth-header)
        request-opts {:headers {"Authorization" auth-header}
                      :throw   false}
        request-opts (if body-params
                       (assoc request-opts
                              :headers (assoc (:headers request-opts)
                                              "Content-Type" "application/x-www-form-urlencoded")
                              :body body-params)
                       request-opts)]
    (try
      (case method
        :get    (http/get url request-opts)
        :post   (http/post url request-opts)
        :delete (http/delete url request-opts))
      (catch Exception e
        (log/error e (str "Exception during Mailgun " (name method) " request to " url))
        {:error       true
         :exception   (.getMessage e)
         :stack-trace (with-out-str (.printStackTrace e))}))))

;; Generate a random CSRF token
(defn generate-csrf-token []
  (let [random-bytes (byte-array 32)]
    (.nextBytes (java.security.SecureRandom.) random-bytes)
    (.encodeToString (java.util.Base64/getEncoder) random-bytes)))

;; Extract CSRF token from cookies
(defn extract-csrf-from-cookie [cookies]
  (when-let [cookie-str cookies]
    (some->> (re-find #"csrf_token=([^;]+)" cookie-str)
             second)))

;; UI Strings with internationalization (i18n) support
(def ui-strings
  {:en
   {:page
    {:title      "Email subscription"
     :heading    "Subscribe to our mailing list"
     :subheading "Join our community to receive updates and news"}
    :form
    {:email-placeholder  "you@example.com"
     :website-label      "Website (leave this empty)"
     :subscribe-button   "Subscribe"
     :unsubscribe-button "Unsubscribe"}
    :messages
    {:thank-you                  "Thank you!"
     :success-subscribe          "Your email <strong>%s</strong> has been successfully subscribed to our mailing list."
     :already-subscribed         "Already subscribed"
     :already-subscribed-message "The email <strong>%s</strong> is already subscribed to our mailing list."
     :unsubscribed               "Successfully unsubscribed"
     :unsubscribed-message       "The email <strong>%s</strong> has been unsubscribed from our mailing list."
     :not-subscribed             "Warning: not subscribed"
     :not-subscribed-message     "The email <strong>%s</strong> is not currently subscribed to our mailing list. No action was taken."
     :operation-failed           "Operation failed"
     :no-email                   "No email address provided. Please try again."
     :no-email-debug             "Missing email address in form submission."
     :rate-limit                 "Rate limit exceeded"
     :rate-limit-message         "Too many subscription attempts from your IP address. Please try again later."
     :invalid-email              "Invalid email format"
     :invalid-email-message      "The email <strong>%s</strong> appears to be invalid. Please check the format and try again."
     :spam-detected              "Submission rejected"
     :spam-detected-message      "Your submission has been identified as potential spam and has been rejected."
     :csrf-invalid               "Security validation failed"
     :csrf-invalid-message       "Security token validation failed. This could happen if you used an old form or if your session expired."
     :unknown-action             "Unknown action requested. Please try again."
     :server-error               "An unexpected error occurred. Please try again later."}}
   :fr
   {:page
    {:title      "Abonnement par e-mail"
     :heading    "Abonnement à notre liste de diffusion"
     :subheading "Rejoignez notre liste pour recevoir des nouvelles"}
    :form
    {:email-placeholder  "vous@exemple.com"
     :website-label      "Site web (laissez ce champ vide)"
     :subscribe-button   "Abonnement"
     :unsubscribe-button "Désabonnement"}
    :messages
    {:thank-you                  "Merci !"
     :success-subscribe          "Votre adresse e-mail <strong>%s</strong> a été abonnée avec succès."
     :already-subscribed         "Déjà abonné"
     :already-subscribed-message "L'adresse e-mail <strong>%s</strong> est déjà abonnée."
     :unsubscribed               "Désabonnement réussi"
     :unsubscribed-message       "L'adresse e-mail <strong>%s</strong> a été désabonnée."
     :not-subscribed             "Attention : non abonné"
     :not-subscribed-message     "L'adresse e-mail <strong>%s</strong> n'est pas actuellement abonnée. Aucune action n'a été effectuée."
     :operation-failed           "Échec de l'opération"
     :no-email                   "Aucune adresse e-mail fournie. Veuillez réessayer."
     :no-email-debug             "Adresse e-mail manquante dans le formulaire."
     :rate-limit                 "Limite de Taux Dépassée"
     :rate-limit-message         "Trop de tentatives d'abonnement depuis votre adresse IP. Veuillez réessayer plus tard."
     :invalid-email              "Format d'e-mail invalide"
     :invalid-email-message      "L'adresse e-mail <strong>%s</strong> semble être invalide. Veuillez vérifier le format et réessayer."
     :spam-detected              "Soumission rejetée"
     :spam-detected-message      "Votre soumission a été identifiée comme spam potentiel et a été rejetée."
     :csrf-invalid               "Échec de validation de sécurité"
     :csrf-invalid-message       "La validation du jeton de sécurité a échoué. Cela peut se produire si vous avez utilisé un ancien formulaire ou si votre session a expiré."
     :unknown-action             "Action inconnue demandée. Veuillez réessayer."
     :server-error               "Une erreur inattendue s'est produite. Veuillez réessayer plus tard."}}})

;; Function to read EDN configuration file
(defn read-config-file [file-path]
  (try
    (if (.exists (java.io.File. file-path))
      (let [config-content (slurp file-path)]
        (log/info "Reading configuration from:" file-path)
        (edn/read-string config-content))
      (do
        (log/warn "Configuration file not found:" file-path)
        {}))
    (catch Exception e
      (log/error "Error reading configuration file:" (.getMessage e))
      {})))

;; Function to validate the configuration data
(defn validate-config [config-data]
  (cond
    (not (map? config-data))
    (do (log/error "Invalid configuration: expected a map")
        false)
    (and (:ui-strings config-data)
         (not (map? (:ui-strings config-data))))
    (do (log/error "Invalid configuration: ui-strings should be a map")
        false)
    :else true))

;; Function to merge UI strings from configuration with defaults
;; This gives precedence to config file values
(defn merge-ui-strings! [config-data]
  (if-let [config-ui-strings (:ui-strings config-data)]
    (do
      (alter-var-root #'ui-strings
                      (fn [original]
                        (merge-with (fn [orig new]
                                      (merge-with merge orig new))
                                    original
                                    config-ui-strings)))
      (log/info "Merged UI strings from configuration file"))
    (log/info "No UI strings found in configuration file")))

;; Process configuration file
(defn process-config-file [file-path]
  (when file-path
    (log/info "Using configuration file:" file-path)
    (let [config-data (read-config-file file-path)]
      (when (validate-config config-data)
        (merge-ui-strings! config-data)))))

;; Extract the config path from command-line arguments
(defn extract-config-path [args]
  (let [idx (.indexOf (vec args) "--config")]
    (when (and (>= idx 0) (< (inc idx) (count args)))
      (nth args (inc idx)))))

;; Helper function to get strings for a specific language
(defn get-strings
  ([lang] (get ui-strings lang (get ui-strings default-language)))
  ([] (get-strings default-language)))

;; Simplified function to determine language from request - browser only
(defn determine-language [req]
  (let [accept-language (get-in req [:headers "accept-language"] "")]
    (cond
      ;; Check Accept-Language header for supported languages
      (str/includes? accept-language "fr") :fr
      :else                                default-language)))

(defn get-client-ip [req]
  (or (get-in req [:headers "x-forwarded-for"])
      (get-in req [:headers "x-real-ip"])
      (:remote-addr req)
      "unknown-ip"))

(defn rate-limited? [ip]
  (let [now              (System/currentTimeMillis)
        last-pruned-time (atom (System/currentTimeMillis))
        window-start     (- now rate-limit-window)
        requests         (get @ip-request-log ip [])
        recent-requests  (filter #(>= % window-start) requests)]

    ;; Prune old entries periodically
    (when (> (- now @last-pruned-time) rate-limit-window)
      (swap! ip-request-log (fn [log-map]
                              (reduce-kv (fn [m k v]
                                           (assoc m k (filter #(>= % window-start) v)))
                                         {}
                                         log-map)))
      (reset! last-pruned-time now))

    ;; Update the request log with the current timestamp
    (swap! ip-request-log update ip #(conj (or % []) now))

    ;; Prune old entries every 1000 IP requests
    (when (> (count @ip-request-log) 1000)
      (swap! ip-request-log
             (fn [log-map]
               (reduce-kv (fn [m k v]
                            (assoc m k (filter #(>= % window-start) v)))
                          {}
                          log-map))))

    (> (count recent-requests) max-requests-per-window)))

(defn valid-email? [email]
  (let [pattern    #"^[a-zA-Z0-9][a-zA-Z0-9._%+-]{0,63}@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$"
        max-length 254]
    (and (string? email)
         (<= (count email) max-length)
         (boolean (re-matches pattern email))
         (not (re-find #"\.{2,}|@{2,}|\_{2,}|\-{2,}" email)))))

(defn honeypot-filled? [form-data]
  (not (str/blank? (str (:website form-data)))))

;; Add HTML escaping function for XSS protection
(defn escape-html
  "Escape HTML special characters in a string to prevent XSS attacks."
  [s]
  (when s
    (-> s
        (str/replace "&" "&amp;")
        (str/replace "<" "&lt;")
        (str/replace ">" "&gt;")
        (str/replace "\"" "&quot;")
        (str/replace "'" "&#39;"))))

;; HTML template builder function using UI strings
(defn build-index-html [strings language csrf-token]
  (format "
<!DOCTYPE html>
<html lang=\"%s\">
<head>
  <meta charset=\"UTF-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
  <title>%s</title>
  <link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/@picocss/pico@1/css/pico.min.css\">
  <script src=\"https://unpkg.com/htmx.org@1.9.6\"></script>
  <style>
    /* Additional custom styles */
    .container {
      max-width: 800px;
      padding: 2rem 1rem;
      margin: 0 auto;
    }

    /* Responsive adjustments */
    @media (max-width: 768px) {
      .container {
        width: 100%%;
        padding: 1rem;
      }
    }
    .card {
      padding: 2rem;
      margin-bottom: 1rem;
    }
    .success {
      background-color: var(--card-background-color);
      border-left: 5px solid var(--form-element-valid-border-color);
    }
    .error {
      background-color: var(--card-background-color);
      border-left: 5px solid var(--form-element-invalid-border-color);
    }
    .warning {
      background-color: var(--card-background-color);
      border-left: 5px solid #f0ad4e;
    }
    .debug {
      margin-top: 1rem;
      padding: 1rem;
      background-color: var(--code-background-color);
      border-radius: var(--border-radius);
      font-family: var(--font-family);
      white-space: pre-wrap;
      display: none;
      font-size: 0.85rem;
    }
    .htmx-indicator {
      opacity: 0;
      transition: opacity 200ms ease-in;
    }
    .htmx-request .htmx-indicator {
      opacity: 1;
    }
    .htmx-request.htmx-indicator {
      opacity: 1;
    }
    /* Add button styles */
    button.primary {
      background-color: var(--primary);
      color: var(--primary-inverse);
    }
    button.secondary {
      background-color: var(--secondary);
      color: var(--secondary-inverse);
    }
    .grid {
      gap: 1rem;
    }
    /* Honeypot field - hidden from users but visible to bots */
    .visually-hidden {
      position: absolute;
      left: -9999px;
      height: 1px;
      width: 1px;
      overflow: hidden;
    }
  </style>
</head>
<body>
  <main class=\"container\">
    <article class=\"grid\">
      <div>
        <header>
          <h1>%s</h1>
          <p>%s</p>
        </header>

        <form hx-post=\"%s/subscribe\" hx-target=\"#result\" hx-swap=\"outerHTML\" hx-indicator=\"#loading\">
          <input type=\"email\" id=\"email\" name=\"email\" placeholder=\"%s\" required>

          <!-- CSRF Protection -->
          <input type=\"hidden\" name=\"csrf_token\" value=\"%s\">

          <!-- Honeypot field - bots will fill this out, humans won't see it -->
          <div class=\"visually-hidden\">
            <label for=\"website\">%s</label>
            <input type=\"text\" id=\"website\" name=\"website\" autocomplete=\"off\">
          </div>

          <div class=\"grid\">
            <button type=\"submit\" name=\"action\" value=\"subscribe\" class=\"primary\">%s</button>
            <button type=\"submit\" name=\"action\" value=\"unsubscribe\" class=\"secondary\">%s</button>
          </div>
          <progress id=\"loading\" class=\"htmx-indicator\"></progress>
        </form>
      </div>
    </article>

    <div id=\"result\"></div>
  </main>
</body>
</html>"
          (name language)
          (:title (:page strings))
          (:heading (:page strings))
          (:subheading (:page strings))
          base-path               ;; Add base path to form action
          (:email-placeholder (:form strings))
          csrf-token
          (:website-label (:form strings))
          (:subscribe-button (:form strings))
          (:unsubscribe-button (:form strings))))

;; Result templates using UI strings with HTML escaping
(defn result-template [strings type heading-key message-key & args]
  (let [heading (get-in strings [:messages heading-key])
        message (get-in strings [:messages message-key])]
    (format "
    <div id=\"result\">
      <article class=\"card %s\">
          <h3>%s</h3>
        <p>%s</p>
      </article>
    </div>
    " type heading (if (seq args)
                     (apply format message (map escape-html args))
                     message))))

(defn debug-result-template [strings type heading-key message & debug-info]
  (format "
  <div id=\"result\">
    <article class=\"card %s\">
      <header>
        <h3>%s</h3>
      </header>
      <p>%s</p>
      <div id=\"debug-info\" class=\"debug\">%s</div>
    </article>
  </div>
  " type
          (get-in strings [:messages heading-key])
          (escape-html message)
          (escape-html (str debug-info))))

;; Result functions
(defn rate-limit-result [strings]
  (result-template strings "error" :rate-limit :rate-limit-message))

(defn invalid-email-result [strings email]
  (result-template strings "error" :invalid-email :invalid-email-message email))

(defn spam-detected-result [strings]
  (result-template strings "error" :spam-detected :spam-detected-message))

(defn csrf-invalid-result [strings]
  (result-template strings "error" :csrf-invalid :csrf-invalid-message))

(defn success-result [strings email]
  (result-template strings "success" :thank-you :success-subscribe email))

(defn already-subscribed-result [strings email]
  (result-template strings "success" :already-subscribed :already-subscribed-message email))

(defn unsubscribed-result [strings email]
  (result-template strings "success" :unsubscribed :unsubscribed-message email))

(defn not-subscribed-result [strings email]
  (result-template strings "warning" :not-subscribed :not-subscribed-message email))

(defn error-result [strings message debug-info]
  (debug-result-template strings "error" :operation-failed message debug-info))

;; Updated check-if-subscribed function
(defn check-if-subscribed [email]
  (log/info "Checking if email is already subscribed:" email)
  (let [url      (get-mailgun-member-url email)
        _        (log/debug "Making request to check subscription status:" url)
        response (make-mailgun-request :get url nil)]
    (log/debug "Mailgun API check response status:" (:status response))
    (log/debug "Mailgun API check response body:" (:body response))
    (and (not (:error response))
         (= 200 (:status response)))))

;; Updated unsubscribe-from-mailgun function
(defn unsubscribe-from-mailgun [email]
  (log/info "Attempting to unsubscribe email:" email)

  (let [url      (get-mailgun-member-url email)
        _        (log/debug "Making DELETE request to Mailgun API:" url)
        response (make-mailgun-request :delete url nil)]

    (log/debug "Mailgun API unsubscribe response status:" (:status response))
    (log/debug "Mailgun API unsubscribe response body:" (:body response))

    (cond
      (:error response)
      {:success false
       :message "Connection error. Please try again later."
       :debug   response}

      (< (:status response) 300)
      (do
        (log/info "Successfully unsubscribed email:" email)
        {:success true})

      (= (:status response) 404)
      (do
        (log/info "Email not found for unsubscription:" email)
        {:success   false
         :not_found true
         :message   "Email address not found in subscription list."})

      :else
      (do
        (log/error "Failed to unsubscribe email:" email "- Status:" (:status response))
        (log/error "Error response:" (:body response))
        {:success false
         :message "Failed to unsubscribe. Please try again later."
         :debug   {:status (:status response)
                   :body   (:body response)}}))))

;; Updated subscribe-to-mailgun function
(defn subscribe-to-mailgun [email]
  (log/info "Attempting to subscribe email:" email)

  (let [url         (get-mailgun-members-url)
        body-params (format "address=%s&subscribed=yes&upsert=yes"
                            (java.net.URLEncoder/encode email "UTF-8"))
        _           (log/debug "Making request to Mailgun API:" url)
        _           (log/debug "Body:" body-params)
        response    (make-mailgun-request :post url body-params)]

    (log/debug "Mailgun API response status:" (:status response))
    (log/debug "Mailgun API response body:" (:body response))

    (cond
      (:error response)
      {:success false
       :message "Connection error. Please try again later."
       :debug   response}

      (< (:status response) 300)
      (do
        (log/info "Successfully subscribed email:" email)
        {:success true})

      :else
      (do
        (log/error "Failed to subscribe email:" email "- Status:" (:status response))
        (log/error "Error response:" (:body response))
        {:success false
         :message "Failed to subscribe. Please try again later."
         :debug   {:status (:status response)
                   :body   (:body response)}}))))

;; Function to normalize URI for path matching
(defn normalize-uri [uri]
  (let [uri-without-base (if (and (not (str/blank? base-path))
                                  (str/starts-with? uri base-path))
                           (let [path (subs uri (count base-path))]
                             (if (str/blank? path) "/" path))
                           uri)]
    (log/debug "Normalized URI from" uri "to" uri-without-base)
    uri-without-base))

;; Request handlers
(defn handle-index [req]
  (let [lang       (determine-language req)
        strings    (get-strings lang)
        csrf-token (generate-csrf-token)]
    {:status  200
     :headers {"Content-Type" "text/html"
               "Set-Cookie"   (format "csrf_token=%s; Path=%s; HttpOnly; SameSite=Strict"
                                      csrf-token
                                      (if (str/blank? base-path) "/" base-path))}
     :body    (build-index-html strings lang csrf-token)}))

(defn parse-form-data [request]
  (try
    (log/debug "Request method:" (:request-method request))
    (log/debug "Headers:" (pr-str (select-keys (:headers request) ["content-type" "content-length"])))
    (if-let [body-stream (:body request)]
      (try
        (let [body (slurp body-stream)]
          (log/debug "Raw body content:" body)

          ;; Parse the body in the most robust way possible
          (let [result (reduce (fn [acc pair]
                                 (if-let [[_ k v] (re-matches #"([^=]+)=(.*)" pair)]
                                   (try
                                     (assoc acc (keyword k) (java.net.URLDecoder/decode v "UTF-8"))
                                     (catch Throwable t
                                       (log/error "Decoding error for" k "=" v ":" (str t))
                                       (assoc acc (keyword k) v)))
                                   acc))
                               {}
                               (str/split body #"&"))]
            (log/debug "Parsed form data:" (pr-str result))
            result))
        (catch Throwable t (log/error "Error reading body:" (str t)) {}))
      (do (log/debug "No body in request") {}))

    (catch Throwable t
      (log/error "Top-level error in parse-form-data:" (str t))
      (log/error "Stack trace:" (with-out-str (.printStackTrace t)))
      {})))

;; Helper function to parse query params from URI (using the same robust approach)
(defn parse-query-params [uri]
  (try
    (if-let [query-string (second (str/split uri #"\?"))]
      (reduce (fn [acc pair]
                (if-let [[_ k v] (re-matches #"([^=]+)=(.*)" pair)]
                  (try
                    (assoc acc (keyword k) (java.net.URLDecoder/decode v "UTF-8"))
                    (catch Throwable t
                      (log/error "Decoding error for query param" k "=" v ":" (str t))
                      (assoc acc (keyword k) v)))
                  acc))
              {}
              (str/split query-string #"&"))
      {})
    (catch Throwable t (log/error "Error parsing query params:" (str t)) {})))

;; Handle subscription with robust form data parsing
(defn handle-subscribe [req]
  (log/info "Received subscription request")
  (log/debug "Request method:" (:request-method req))
  (log/debug "Headers:" (pr-str (:headers req)))

  (try
    (let [form-data         (parse-form-data req)
          email             (-> (:email form-data) str/trim str/lower-case)
          action            (or (:action form-data) "subscribe")
          client-ip         (get-client-ip req)
          lang              (determine-language req)
          strings           (get-strings lang)
          form-csrf-token   (:csrf_token form-data)
          cookie-csrf-token (extract-csrf-from-cookie (get-in req [:headers "cookie"]))]

      (log/debug "Parsed form data:" (pr-str form-data))
      (log/debug "Email from form:" email)
      (log/debug "Action from form:" action)
      (log/debug "CSRF token from form:" form-csrf-token)
      (log/debug "CSRF token from cookie:" cookie-csrf-token)

      ;; CSRF Protection check
      (if (or (nil? form-csrf-token)
              (nil? cookie-csrf-token)
              (not= form-csrf-token cookie-csrf-token))
        (do
          (log/warn "CSRF token validation failed")
          (log/warn "Form token:" form-csrf-token)
          (log/warn "Cookie token:" cookie-csrf-token)
          {:status  403
           :headers {"Content-Type" "text/html"}
           :body    (csrf-invalid-result strings)})

        ;; Anti-spam: rate limiting
        (if (rate-limited? client-ip)
          (do
            (log/warn "Rate limit exceeded for IP:" client-ip)
            {:status  429
             :headers {"Content-Type" "text/html"
                       "Retry-After"  "3600"}
             :body    (rate-limit-result strings)})

          ;; Anti-spam: honeypot check
          (if (honeypot-filled? form-data)
            (do
              (log/warn "Spam detected: honeypot field filled from IP:" client-ip)
              {:status  400
               :headers {"Content-Type" "text/html"}
               :body    (spam-detected-result strings)})

            ;; Email validation
            (cond
              (str/blank? email)
              (do
                (log/error "No email provided in request")
                (log/error "Form data:" (pr-str form-data))
                {:status  400
                 :headers {"Content-Type" "text/html"}
                 :body    (error-result
                           strings
                           (get-in strings [:messages :no-email])
                           (str "Request method: " (name (:request-method req)) "\n"
                                "Headers: " (pr-str (:headers req)) "\n"
                                "Form data: " (pr-str form-data)))})

              ;; Anti-spam: Email format validation
              (not (valid-email? email))
              (do
                (log/error "Invalid email format:" email)
                {:status  400
                 :headers {"Content-Type" "text/html"}
                 :body    (invalid-email-result strings email)})

              ;; Valid request, proceed with normal handling
              :else
              (case action
                "subscribe"
                (if (check-if-subscribed email)
                  (do
                    (log/info "Email already subscribed:" email)
                    {:status  200
                     :headers {"Content-Type" "text/html"}
                     :body    (already-subscribed-result strings email)})

                  ;; If not subscribed, proceed with subscription
                  (let [result (subscribe-to-mailgun email)]
                    (if (:success result)
                      {:status  200
                       :headers {"Content-Type" "text/html"}
                       :body    (success-result strings email)}
                      {:status  400
                       :headers {"Content-Type" "text/html"}
                       :body    (error-result
                                 strings
                                 (or (:message result) (get-in strings [:messages :server-error]))
                                 (str "Debug info:\n" (pr-str (:debug result))))})))

                "unsubscribe"
                (if (not (check-if-subscribed email))
                  (do
                    (log/info "Email not subscribed, can't unsubscribe:" email)
                    {:status  200 ; Changed from 404 to 200 for better user experience
                     :headers {"Content-Type" "text/html"}
                     :body    (not-subscribed-result strings email)})

                  ;; If subscribed, proceed with unsubscription
                  (let [result (unsubscribe-from-mailgun email)]
                    (if (:success result)
                      {:status  200
                       :headers {"Content-Type" "text/html"}
                       :body    (unsubscribed-result strings email)}
                      (if (:not_found result)
                        {:status  200 ; Changed from 404 to 200 for better user experience
                         :headers {"Content-Type" "text/html"}
                         :body    (not-subscribed-result strings email)}
                        {:status  400
                         :headers {"Content-Type" "text/html"}
                         :body    (error-result
                                   strings
                                   (or (:message result) (get-in strings [:messages :server-error]))
                                   (str "Debug info:\n" (pr-str (:debug result))))}))))

                ;; Default case for unknown action
                (do
                  (log/error "Unknown action requested:" action)
                  {:status  400
                   :headers {"Content-Type" "text/html"}
                   :body    (error-result
                             strings
                             (get-in strings [:messages :unknown-action])
                             (str "Unknown action: " action))})))))))

    (catch Throwable e
      (log/error "Exception in handle-subscribe:" (str e))
      (log/error "Stack trace:" (with-out-str (.printStackTrace e)))
      (let [lang    (determine-language req)
            strings (get-strings lang)]
        {:status  500
         :headers {"Content-Type" "text/html"}
         :body    (error-result
                   strings
                   (get-in strings [:messages :server-error])
                   (str "Exception: " (.getMessage e) "\n\n"
                        "Stack trace:\n" (with-out-str (.printStackTrace e))))}))))

;; Debug endpoint
(defn handle-debug [req]
  (log/info "Serving debug page")
  (let [lang       (determine-language req)
        debug-info {:env        {:mailgun-list-id      mailgun-list-id
                                 :mailgun-api-endpoint mailgun-api-endpoint
                                 :mailgun-api-key      "****"
                                 :base-path            base-path}
                    :i18n       {:current-language    (name lang)
                                 :available-languages (keys ui-strings)
                                 :browser-language    (get-in req [:headers "accept-language"])}
                    :req        {:uri     (:uri req)
                                 :method  (:request-method req)
                                 :headers (:headers req)}
                    :rate-limit {:window-length (str (/ rate-limit-window 1000) " seconds")
                                 :max-requests  max-requests-per-window
                                 :current-log   (count @ip-request-log)}}]
    {:status  200
     :headers {"Content-Type" "application/json"}
     :body    (json/generate-string debug-info {:pretty true})}))

;; Main app with routes
(defn app [req]
  (let [uri             (:uri req)
        normalized-uri  (normalize-uri uri)
        query-params    (parse-query-params uri)
        req-with-params (assoc req :query-params query-params)]
    (try
      (log/debug "Processing request:" (:request-method req) uri)
      (log/debug "Normalized path:" normalized-uri)

      (case [(:request-method req) normalized-uri]
        [:get "/"]           (handle-index req-with-params)
        [:post "/subscribe"] (handle-subscribe req-with-params)
        [:get "/debug"]      (handle-debug req-with-params)
        (do
          (log/info "Not found:" (:request-method req) uri)
          {:status  404
           :headers {"Content-Type" "text/html"}
           :body    (format "<h1>%s</h1><p>%s: %s %s</p>"
                            "Not Found"
                            "Resource not found"
                            (name (:request-method req))
                            uri)}))
      (catch Throwable e
        (log/error "Uncaught exception in request handler:" (str e))
        (log/error "Stack trace:" (with-out-str (.printStackTrace e)))
        {:status  500
         :headers {"Content-Type" "text/html"}
         :body    (str "<h1>Internal Server Error</h1><pre>"
                       (.getMessage e) "\n\n"
                       (with-out-str (.printStackTrace e))
                       "</pre>")}))))

;; Start server
(defn start-server [& [port]]
  (let [port (or port 8080)]
    (log/info (str "Starting server on http://localhost:" port))
    (log/info (str "Base path: " (if (str/blank? base-path) "[root]" base-path)))
    (server/run-server app {:port port})))

;; Main entry point
(when (= *file* (System/getProperty "babashka.file"))
  (let [args        *command-line-args*
        ;; Check if first argument is a valid port number
        port        (if (and (seq args)
                             (try (Integer/parseInt (first args)) true
                                  (catch NumberFormatException _ false)))
                      (Integer/parseInt (first args))
                      8080)
        config-path (extract-config-path args)]

    ;; Process configuration file if provided
    (when config-path
      (process-config-file config-path))

    (if-not (and mailgun-list-id
                 mailgun-api-endpoint
                 mailgun-api-key)
      (log/error "Missing environment variable")
      (do (log/info (str "Starting server on http://localhost:" port))
          (log/info (str "Base path: " (if (str/blank? base-path) "[root]" base-path)))
          (server/run-server app {:port port})
          ;; Keep the server running
          @(promise)))))

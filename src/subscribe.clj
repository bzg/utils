#!/usr/bin/env bb

;; Copyright (c) DINUM, Bastien Guerry
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: EPL-2.0.txt

;; This script runs a web application to let users subscribe to a
;; Mailgun mailing list. You need a Mailgun API key.
;;
;; You need to set these environment variables:
;;
;; export MAILGUN_LIST_ID="my@list.com"
;; export MAILGUN_API_ENDPOINT="https://api.eu.mailgun.net/v3"
;; export MAILGUN_API_KEY="xxxxxxxxxxxxxxxx-xxxxxxxx-xxxxxxxx"
;;
;; By default, the application runs on http://localhost:8080:
;;
;; ~$ subscribe
;;
;; You can also set a base path to deploy the application on a
;; subdirectory (e.g. "http://localhost:8080/newsletter"):
;;
;; export SUBSCRIBE_BASE_PATH="/newsletter"
;;
;; You can use a EDN configuration file for setting options:
;;
;; ~$ subscribe --config config.edn
;;
;; This config file can let you set/override these variables:
;;
;; - default-language
;; - ui-strings
;; - mailgun-api-endpoint
;; - mailgun-list-id
;; - base-path
;;
;; ~$ subscribe -h # Show more information

(require '[org.httpkit.server :as server]
         '[babashka.http-client :as http]
         '[clojure.string :as str]
         '[cheshire.core :as json]
         '[taoensso.timbre :as log]
         '[clojure.edn :as edn]
         '[babashka.cli :as cli])

(def version "0.2")

(defn print-version []
  (println (format "subscribe %s" version))
  (System/exit 0))

(def cli-options
  {:help      {:alias :h :desc "Display help" :type :boolean}
   :config    {:alias :c :desc "Config file path" :ref "<file>"}
   :port      {:alias :p :desc "Port number" :ref "<port>" :default 8080 :coerce :int}
   :base-path {:alias :b :desc "Base path" :ref "<path>"}
   :log-level {:alias :l :desc "Log level" :ref "<level>" :default :info}
   :version   {:alias :v :desc "Describe version" :type :boolean }})

(defn print-usage []
  (println "Usage: subscribe [options]")
  (println "\nOptions:")
  (doseq [[k v] cli-options]
    (println (format "  --%s, -%s %s %s"
                     (name k)
                     (name (:alias v))
                     (or (:ref v) "")
                     (:desc v))))
  (println "\nEnvironment variables:")
  (println "  MAILGUN_LIST_ID          Mailgun list identifier (if not provided with -l)")
  (println "  MAILGUN_API_ENDPOINT     Mailgun API endpoint")
  (println "  MAILGUN_API_KEY          Mailgun API key")
  (println "  SUBSCRIBE_BASE_PATH      Base path for deployments in subdirectories")
  (println "\nExamples:")
  (println "  subscribe                # Run on default port 8080")
  (println "  subscribe -p 4444        # Run on port 4444")
  (println "  subscribe -l :debug      # Specify log level as :debug")
  (println "  subscribe -b /app        # Set base path to /app")
  (println "  subscribe -c config.edn  # Load configuration from file")
  (System/exit 0))

;; Defaults
(def default-language :en)

;; Anti-Spam protections
(def rate-limit-window (* 60 60 1000)) ;; 1 hour in milliseconds
(def max-requests-per-window 10) ;; Maximum 10 requests per IP per hour
(def ip-request-log (atom {}))
(def last-pruned-time (atom (System/currentTimeMillis)))

;; Session store for CSRF tokens
(def session-store (atom {}))

;; Set up environment variables
(def mailgun-list-id
  (or (System/getenv "MAILGUN_LIST_ID")
      (log/error "Missing MAILGUN_LIST_ID")))
(def mailgun-api-endpoint
  (or (System/getenv "MAILGUN_API_ENDPOINT")
      "https://api.mailgun.net/v3"))
(def mailgun-api-key
  (or (System/getenv "MAILGUN_API_KEY")
      (log/error "Missing MAILGUN_API_KEY")))

;; Function to normalize base-path handling
(defn normalize-base-path [path]
  (if (str/blank? path)
    ""
    (let [path-without-trailing (if (str/ends-with? path "/")
                                  (str/replace path #"/$" "")
                                  path)
          path-with-leading     (if (str/starts-with? path-without-trailing "/")
                                  path-without-trailing
                                  (str "/" path-without-trailing))]
      path-with-leading)))

;; Base path configuration for subdirectory deployments
(def base-path
  (let [path (or (System/getenv "SUBSCRIBE_BASE_PATH") "")]
    (normalize-base-path path)))

;; Helper function to construct paths with the base path
(defn make-path [& segments]
  (let [segments (remove str/blank? segments)]
    (if (str/blank? base-path)
      ;; If no base path, just join segments with "/"
      (str "/" (str/join "/" segments))
      ;; If base path exists, handle special case when base-path ends
      ;; with a segment name
      (let [base-segment (last (str/split base-path #"/"))]
        (if (and (= 1 (count segments))
                 (= base-segment (first segments)))
          ;; If the only segment matches the last part of base-path,
          ;; avoid duplication
          base-path
          ;; Otherwise append segments properly
          (str base-path
               (when (not (str/starts-with? (first segments) "/"))
                 "/")
               (str/join "/" segments)))))))

;; Returns Authorization header value for Mailgun API requests
(def get-mailgun-auth-header
  (memoize
   (fn []
     (let [auth-string  (str "api:" mailgun-api-key)
           auth-bytes   (.getBytes auth-string)
           encoder      (java.util.Base64/getEncoder)
           encoded-auth (.encodeToString encoder auth-bytes)]
       (str "Basic " encoded-auth)))))

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
  (format "%s/lists/%s/members" mailgun-api-endpoint mailgun-list-id))

(defn make-mailgun-request
  "Makes a request to the Mailgun API with appropriate authentication"
  [method url body-params]
  (let [auth-header  (get-mailgun-auth-header)
        request-opts (cond-> {:headers {"Authorization" auth-header} :throw false}
                       body-params
                       (assoc :headers {"Authorization" auth-header
                                        "Content-Type"  "application/x-www-form-urlencoded"}
                              :body body-params))
        http-fn      (get {:get http/get :post http/post :delete http/delete} method)]
    (try
      (http-fn url request-opts)
      (catch Exception e
        (log/error e (str "Mailgun " (name method) " error: " url))
        {:error       true
         :exception   (.getMessage e)
         :stack-trace (with-out-str (.printStackTrace e))}))))

;; Generate a random CSRF token
(defn generate-csrf-token []
  (let [random-bytes (byte-array 32)]
    (.nextBytes (java.security.SecureRandom.) random-bytes)
    (.encodeToString (java.util.Base64/getEncoder) random-bytes)))

;; Get or create a token for a session
(defn get-or-create-csrf-token [ip]
  (or (get @session-store ip)
      (let [token (generate-csrf-token)]
        (swap! session-store assoc ip token)
        (log/debug "Generated new CSRF token for IP" ip ":" token)
        token)))

;; Extract CSRF token from cookies - kept for backward compatibility
(defn extract-csrf-from-cookie [cookies]
  (when-let [cookie-str cookies]
    (some->> (re-find #"csrf_token=([^;]+)" cookie-str) second)))

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
        (edn/read-string {:readers {}} config-content))
      (log/warn "Configuration file not found:" file-path))
    (catch Exception e
      (log/error "Error reading configuration file:" (.getMessage e)))))

;; Function to validate the configuration data
(defn validate-config [config-data]
  (let [and-not   #(when-let [r (get config-data %1)] (not (apply %2 [r])))
        log-false #(do (log/error %) false)]
    (cond
      (not (map? config-data))
      (log-false "Invalid configuration: expected a map")
      (and-not :default-language keyword?)
      (log-false "Invalid configuration: default-language should be a keyword")
      (and-not :ui-strings map?)
      (log-false "Invalid configuration: ui-strings should be a map")
      (and-not :mailgun-list-id string?)
      (log-false "Invalid configuration: mailgun-list-id should be a string")
      (and-not :mailgun-api-endpoint string?)
      (log-false "Invalid configuration: mailgun-api-endpoint should be a string")
      (and-not :base-path string?)
      (log-false "Invalid configuration: base-path should be a string")
      :else true)))

(defn apply-config-overrides! [config-data]
  ;; Override default-language if specified
  (when-let [lang (:default-language config-data)]
    (alter-var-root #'default-language (constantly lang))
    (log/info "Overriding default-language from config:" lang))
  ;; Override mailgun-list-id if specified
  (when-let [list (:mailgun-list-id config-data)]
    (alter-var-root #'mailgun-list-id (constantly list))
    (log/info "Overriding mailgun-list-id from config:" list))
  ;; Override mailgun-api-endpoint if specified
  (when-let [endpoint (:mailgun-api-endpoint config-data)]
    (alter-var-root #'mailgun-api-endpoint (constantly endpoint))
    (log/info "Overriding mailgun-api-endpoint from config:" endpoint))
  ;; Override base-path if specified
  (when-let [path (:base-path config-data)]
    (alter-var-root #'base-path (constantly (normalize-base-path path)))
    (log/info "Overriding base-path from config:" path)))

;; Function to merge UI strings from configuration with defaults
;; This gives precedence to config file values
(defn merge-ui-strings! [config-data]
  (if-let [config-ui-strings (:ui-strings config-data)]
    (do (alter-var-root
         #'ui-strings
         (fn [original]
           (merge-with (fn [orig new]
                         (merge-with merge orig new))
                       original
                       config-ui-strings)))
        (log/info "Merged UI strings from configuration file"))
    (log/info "No UI strings found in configuration file")))

(defn process-config-file [file-path]
  (when file-path
    (log/info "Using configuration file:" file-path)
    (let [config-data (read-config-file file-path)]
      (when (validate-config config-data)
        ;; Apply both UI string merging and variable overrides
        (merge-ui-strings! config-data)
        (apply-config-overrides! config-data)))))

;; Extract the config path from command-line arguments
(defn extract-config-path [args]
  (let [idx (.indexOf (vec args) "--config")]
    (when (and (>= idx 0) (< (inc idx) (count args)))
      (nth args (inc idx)))))

;; Helper function to get strings for a specific language
(defn get-strings
  ([lang] (get ui-strings lang (get ui-strings default-language)))
  ([] (get-strings default-language)))

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
  (let [now             (System/currentTimeMillis)
        window-start    (- now rate-limit-window)
        requests        (get @ip-request-log ip [])
        recent-requests (filter #(>= % window-start) requests)]
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
  (let [pattern    #"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$"
        max-length 254]
    (and (string? email)
         (<= (count email) max-length)
         (boolean (re-matches pattern email))
         (not (re-find #"\.{2,}|@{2,}|\_{2,}|\-{2,}" email)))))

(defn honeypot-filled? [form-data]
  (not (str/blank? (str (:website form-data)))))

;; Add HTML escaping function for XSS protection
(defn escape-html [^String s]
  (when (not-empty s)
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
  <link rel=\"icon\" href=\"data:image/png;base64,iVBORw0KGgo=\">
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
        <form hx-post=\"%s\" hx-target=\"#result\" hx-swap=\"outerHTML\" hx-indicator=\"#loading\">
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
          (make-path "subscribe")
          (:email-placeholder (:form strings))
          csrf-token
          (:website-label (:form strings))
          (:subscribe-button (:form strings))
          (:unsubscribe-button (:form strings))))

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

(def security-headers
  {"X-Content-Type-Options"  "nosniff"
   "X-Frame-Options"         "DENY"
   "Content-Security-Policy" "default-src 'self'; script-src 'self' 'unsafe-inline' https://unpkg.com; style-src 'self' 'unsafe-inline' https://cdn.jsdelivr.net; img-src 'self' data:;"})

(def security-headers-self
  {"X-Content-Type-Options"  "nosniff"
   "X-Frame-Options"         "DENY"
   "Content-Security-Policy" "default-src 'self';"})

(defn make-response [status type strings heading-key message-key & args]
  {:status  status
   :headers (merge {"Content-Type" "text/html; charset=UTF-8"} security-headers)
   :body    (apply result-template strings type heading-key message-key args)})

(defn handle-error [req e debug-info]
  (log/error "Error:" (str e))
  (log/error "Stack trace:" (with-out-str (.printStackTrace e)))
  (let [lang    (determine-language req)
        strings (get-strings lang)]
    {:status  500
     :headers (merge {"Content-Type" "text/html; charset=UTF-8"} security-headers)
     :body    (debug-result-template
               strings
               "error"
               :operation-failed
               (get-in strings [:messages :server-error])
               (str "Exception: " (.getMessage e) "\n\n"
                    "Debug info:\n" debug-info))}))

(defn check-if-subscribed [email]
  (log/info "Checking if email is already subscribed:" email)
  (let [url      (get-mailgun-member-url email)
        _        (log/debug "Making request to check subscription status:" url)
        response (make-mailgun-request :get url nil)]
    (log/debug "Mailgun API check response status:" (:status response))
    (log/debug "Mailgun API check response body:" (:body response))
    (and (not (:error response))
         (= 200 (:status response)))))

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

(def subscription-count (atom 0))
(defn warn-new-subscription! []
  (let [new-count (swap! subscription-count inc)]
    (when (zero? (mod new-count 10))
      (log/info (format "%d new subscriptions" new-count)))))

(defn subscribe-to-mailgun [email]
  (log/info "Attempting to subscribe email:" email)
  (if (check-if-subscribed email)
    ;; Already subscribed case
    {:success true, :already_subscribed true}
    ;; Not subscribed yet - proceed with subscription
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
        (do (warn-new-subscription!)
            (log/info "Successfully subscribed email:" email)
            {:success true})
        :else
        (do
          (log/error "Failed to subscribe email:" email "- Status:" (:status response))
          (log/error "Error response:" (:body response))
          {:success false
           :message "Failed to subscribe. Please try again later."
           :debug   {:status (:status response)
                     :body   (:body response)}})))))

(defn normalize-uri [uri]
  (let [uri-without-base
        (if (and (not (str/blank? base-path))
                 (or (= uri base-path)
                     (and (str/starts-with? uri (str base-path "/"))
                          (> (count uri) (inc (count base-path))))))
          (let [path (subs uri (count base-path))]
            ;; Add this check to ensure we have a leading slash
            (cond
              (str/blank? path)           "/"
              (str/starts-with? path "/") path
              :else                       (str "/" path)))
          uri)]
    (log/debug "Normalized URI from" uri "to" uri-without-base)
    uri-without-base))

;; Request handlers - modified to use session-based CSRF
(defn handle-index [req]
  (let [lang       (determine-language req)
        strings    (get-strings lang)
        client-ip  (get-client-ip req)
        csrf-token (get-or-create-csrf-token client-ip)]
    (log/debug "Using CSRF token for IP" client-ip ":" csrf-token)
    {:status  200
     :headers (merge {"Content-Type" "text/html; charset=UTF-8"} security-headers)
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
        (catch Throwable t (log/error "Error reading body:" (str t))))
      (log/debug "No body in request"))
    (catch Throwable t
      (log/error "Top-level error in parse-form-data:" (str t))
      (log/error "Stack trace:" (with-out-str (.printStackTrace t))))))

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
              (str/split query-string #"&")))
    (catch Throwable t (log/error "Error parsing query params:" (str t)))))

(defn process-subscription-action [strings action email]
  (case action
    "subscribe"
    (let [result (subscribe-to-mailgun email)]
      (cond
        (and (:success result) (:already_subscribed result))
        (make-response 200 "success" strings :already-subscribed :already-subscribed-message email)
        (:success result)
        (make-response 200 "success" strings :thank-you :success-subscribe email)
        :else
        {:status  400
         :headers (merge {"Content-Type" "text/html; charset=UTF-8"} security-headers)
         :body    (debug-result-template
                   strings
                   "error"
                   :operation-failed
                   (or (:message result) (get-in strings [:messages :server-error]))
                   (str "Debug info:\n" (pr-str (:debug result))))}))
    "unsubscribe"
    (let [result (unsubscribe-from-mailgun email)]
      (cond
        (:success result)
        (make-response 200 "success" strings :unsubscribed :unsubscribed-message email)
        (:not_found result)
        (make-response 200 "warning" strings :not-subscribed :not-subscribed-message email)
        :else
        {:status  400
         :headers (merge {"Content-Type" "text/html; charset=UTF-8"} security-headers)
         :body    (debug-result-template
                   strings
                   "error"
                   :operation-failed
                   (or (:message result) (get-in strings [:messages :server-error]))
                   (str "Debug info:\n" (pr-str (:debug result))))}))
    ;; Default case for unknown action
    (make-response 400 "error" strings :unknown-action :unknown-action)))

;; Handle subscription with session-based CSRF validation
(defn handle-subscribe [req]
  (log/info "Received subscription request")
  (log/debug "Request method:" (:request-method req))
  (log/debug "Headers:" (pr-str (:headers req)))
  (try
    (let [form-data       (parse-form-data req)
          email           (-> (:email form-data) str/trim str/lower-case)
          action          (or (:action form-data) "subscribe")
          client-ip       (get-client-ip req)
          lang            (determine-language req)
          strings         (get-strings lang)
          form-csrf-token (:csrf_token form-data)
          session-token   (get @session-store client-ip)]

      (log/debug "Parsed form data:" (pr-str form-data))
      (log/debug "Email from form:" email)
      (log/debug "Action from form:" action)
      (log/debug "CSRF token from form:" form-csrf-token)
      (log/debug "Session CSRF token:" session-token)

      ;; CSRF Protection check
      (if (or (nil? form-csrf-token)
              (nil? session-token)
              (not= form-csrf-token session-token))
        (do
          (log/warn "CSRF token validation failed")
          (log/warn "Form token:" form-csrf-token)
          (log/warn "Session token:" session-token)
          (make-response 403 "error" strings :csrf-invalid :csrf-invalid-message))

        ;; Anti-spam: rate limiting
        (if (rate-limited? client-ip)
          (do
            (log/warn "Rate limit exceeded for IP:" client-ip)
            {:status  429
             :headers (merge {"Content-Type" "text/html; charset=UTF-8"
                              "Retry-After"  "3600"}
                             security-headers)
             :body    (result-template strings "error" :rate-limit :rate-limit-message)})

          ;; Anti-spam: honeypot check
          (if (honeypot-filled? form-data)
            (do
              (log/warn "Spam detected: honeypot field filled from IP:" client-ip)
              (make-response 400 "error" strings :spam-detected :spam-detected-message))

            ;; Email validation
            (cond
              (str/blank? email)
              (do
                (log/error "No email provided in request")
                (log/error "Form data:" (pr-str form-data))
                {:status  400
                 :headers (merge {"Content-Type" "text/html; charset=UTF-8"} security-headers)
                 :body    (debug-result-template
                           strings
                           "error"
                           :operation-failed
                           (get-in strings [:messages :no-email])
                           (str "Request method: " (name (:request-method req)) "\n"
                                "Headers: " (pr-str (:headers req)) "\n"
                                "Form data: " (pr-str form-data)))})

              ;; Anti-spam: Email format validation
              (not (valid-email? email))
              (do
                (log/error "Invalid email format:" email)
                (make-response 400 "error" strings :invalid-email :invalid-email-message email))

              ;; Valid request, proceed with normal handling
              :else
              (process-subscription-action strings action email))))))
    (catch Throwable e
      (handle-error req e (str "Request method: " (name (:request-method req)) "\n"
                               "Headers: " (pr-str (:headers req)))))))

;; Debug endpoint - updated to include session info
(defn handle-debug [req]
  (log/info "Serving debug page")
  (let [lang      (determine-language req)
        client-ip (get-client-ip req)
        debug-info
        {:env        {:mailgun-list-id      mailgun-list-id
                      :mailgun-api-endpoint mailgun-api-endpoint
                      :mailgun-api-key      "****"
                      :base-path            base-path}
         :i18n       {:current-language    (name lang)
                      :available-languages (keys ui-strings)
                      :browser-language    (get-in req [:headers "accept-language"])}
         :session    {:client-ip     client-ip
                      :csrf-token    (get @session-store client-ip)
                      :session-count (count @session-store)}
         :req        {:uri     (:uri req)
                      :method  (:request-method req)
                      :headers (:headers req)}
         :rate-limit {:window-length (str (/ rate-limit-window 1000) " seconds")
                      :max-requests  max-requests-per-window
                      :current-log   (count @ip-request-log)}}]
    {:status  200
     :headers (merge {"Content-Type" "application/json; charset=UTF-8"}
                     security-headers-self)
     :body    (json/generate-string debug-info {:pretty true})}))

(defn handle-robots-txt []
  {:status  200
   :headers {"Content-Type" "text/plain; charset=UTF-8"}
   :body    "User-agent: *\nDisallow: /"})

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
        [:get "/robots.txt"] (handle-robots-txt)
        (do
          (log/debug "Not found:" (:request-method req) uri)
          {:status  404
           :headers (merge {"Content-Type" "text/html; charset=UTF-8"}
                           security-headers-self)
           :body    (format "<h1>%s</h1><p>%s: %s %s</p>"
                            "Not Found"
                            "Resource not found"
                            (name (:request-method req))
                            uri)}))
      (catch Throwable e
        (handle-error req e (str "URI: " uri))))))

(defn start-server [& [port]]
  (let [port (or port 8080)]
    (log/info (str "Starting server on http://localhost:" port))
    (log/info (str "Base path: " (if (str/blank? base-path) "[root]" base-path)))
    (server/run-server app {:port port})))

;; Main entry point
(when (= *file* (System/getProperty "babashka.file"))
  (let [opts (cli/parse-opts *command-line-args* {:spec cli-options})
        port (get opts :port 8080)]
    ;; Handle help and version option
    (when (:help opts) (print-usage))
    (when (:version opts) (print-version))
    ;; Log configuration
    (log/info "MAILGUN_LIST_ID=" mailgun-list-id)
    (log/info "MAILGUN_API_ENDPOINT=" mailgun-api-endpoint)
    (when (not-empty base-path) (log/info "SUBSCRIBE_BASE_PATH=" base-path))
    (if (not-empty mailgun-api-key)
      (log/info "MAILGUN_API_KEY=****")
      (log/error "MAILGUN_API_KEY not set"))
    ;; Configure Timbre logging
    (when-let [log-level (get opts :log-level)]
      (log/merge-config! {:min-level log-level})
      (log/info "Setting log-level from command line:" log-level))
    ;; Set base-path from command line if provided
    (when-let [path (:base-path opts)]
      (alter-var-root #'base-path (constantly (normalize-base-path path)))
      (log/info "Setting base-path from command line:" path))
    ;; Process configuration file if provided
    (when-let [config-path (:config opts)]
      (process-config-file config-path))
    ;; Start the server
    (log/info (str "Starting server on http://localhost:" port))
    (log/info (str "Base path: " (if (str/blank? base-path) "[root]" base-path)))
    (server/run-server app {:port port})
    ;; Keep the server running
    @(promise)))

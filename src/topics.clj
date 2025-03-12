#!/usr/bin/env bb

;; Copyright (c) DINUM, Bastien Guerry
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: EPL-2.0.txt

;; Topics is a small web application exposing topics, loaded from a
;; local or distance json file.  You can try it quickly like this:
;;
;; ~$ topics -f https://code.gouv.fr/data/faq.json
;;
;; Then check http://localhost:8080
;;
;; Run topics -h for options.
;;
;; Here is an example json with "title", "content" and "path", which
;; last item is used to infer the category:
;;
;; [ {
;;   "title" : "Topic title",
;;   "content" : "<p>Content as HTML",
;;   "path" : [ "Section", "Subsection (as category)" ]
;; } ]

(require '[org.httpkit.server :as server]
         '[cheshire.core :as json]
         '[clojure.string :as str]
         '[babashka.cli :as cli]
         '[taoensso.timbre :as log])

;; Define CLI specs
(def cli-options
  {:port      {:desc    "Port number for server"
               :default 8080
               :alias   :p
               :coerce  :int}
   :topics    {:desc    "Path to Topics JSON file"
               :alias   :f
               :default "topics.json"}
   :source    {:desc    "Path to the Topics source"
               :alias   :s
               :default "https://github.com/bzg/utils"}
   :title     {:desc    "Website title"
               :alias   :t
               :default "Topics"}
   :tagline   {:desc    "Website tagline"
               :alias   :T
               :default "A few topics to explore"}
   :footer    {:desc    "Footer text"
               :alias   :F
               :default "https://github.com/bzg/utils"}
   :base-path {:desc    "Base path for subdirectory deployment (e.g., /topics)"
               :alias   :b
               :default ""}
   :help      {:desc   "Show help"
               :alias  :h
               :coerce :boolean}
   :log-level {:alias   :l
               :desc    "Set log level (debug, info, warn, error)"
               :ref     "<level>"
               :default :info}})

;; Function to append base path to URLs
(defn with-base-path [path base-path]
  (let [clean-base (if (str/ends-with? base-path "/")
                     (subs base-path 0 (dec (count base-path)))
                     base-path)]
    (str clean-base path)))

;; Safely encode URL components to handle special characters
(defn safe-url-encode [s]
  (when s
    (-> s
        (java.net.URLEncoder/encode "UTF-8")
        (str/replace "+" "%20")  ;; Replace + with %20 for spaces
        (str/replace "%28" "(")  ;; Keep common characters readable
        (str/replace "%29" ")")
        (str/replace "%2C" ","))))

;; Safely decode URL components
(defn safe-url-decode [s]
  (when s
    (try
      (java.net.URLDecoder/decode s "UTF-8")
      (catch Exception _
        (log/warn "Error decoding URL parameter:" s)
        s))))  ;; Return original on error

;; Load Topics data directly
(defn load-topics-data [source]
  (try
    (log/info "Loading Topics data from" source)
    (let [content (slurp source)
          data    (json/parse-string content true)]
      (log/info "Loaded" (count data) "Topics items")
      data)
    (catch Exception e
      (log/error "Error loading Topics data from" source ":" (.getMessage e))
      [])))

;; Helper function to strip HTML tags for text content searching
(defn strip-html [html]
  (-> html
      (str/replace #"<[^>]*>" "")
      (str/replace #"&nbsp;" " ")
      (str/replace #"&lt;" "<")
      (str/replace #"&gt;" ">")
      (str/replace #"&amp;" "&")
      (str/replace #"&quot;" "\"")
      (str/replace #"&apos;" "'")))

;; Protect search input by handling potentially harmful characters
(defn sanitize-search-query [query]
  (when query
    (-> query
        (str/replace #"[<>]" "")           ;; Remove < and > characters
        (str/replace #"[\\'\";`]" "")      ;; Remove quotes and other potentially harmful chars
        (str/trim))))                       ;; Trim whitespace

;; Normalize text for improved matching
(defn normalize-text [text]
  (when text
    (-> text
        (str/lower-case)
        ;; Replace diacritical marks
        (str/replace #"[àáâãäå]" "a")
        (str/replace #"[èéêë]" "e")
        (str/replace #"[ìíîï]" "i")
        (str/replace #"[òóôõö]" "o")
        (str/replace #"[ùúûü]" "u")
        (str/replace #"[ýÿ]" "y")
        (str/replace #"[ç]" "c")
        (str/replace #"[œ]" "oe")
        (str/replace #"[æ]" "ae")
        (str/replace #"[ñ]" "n")
        ;; Remove punctuation and special characters
        (str/replace #"[.,;:!?'\"/\\(\\)\\[\\]{}]" " ")
        ;; Collapse multiple spaces
        (str/replace #"\s+" " ")
        (str/trim))))

(defn search-topics [query topics-data]
  (if (or (nil? query) (empty? query))
    []
    (let [sanitized-query  (sanitize-search-query query)
          query-normalized (normalize-text sanitized-query)]
      (filter (fn [item]
                (or
                 ;; Search in title
                 (str/includes? (normalize-text (:title item)) query-normalized)
                 ;; Search in content (stripping HTML tags)
                 (let [content-text (strip-html (:content item))]
                   (str/includes? (normalize-text content-text) query-normalized))
                 ;; Search in category/path
                 (some #(str/includes? (normalize-text %) query-normalized) (:path item))))
              topics-data))))

;; Function to get categories from path
(defn get-categories [topics-data]
  (let [paths      (map :path topics-data)
        categories (distinct (map last paths))]
    (sort categories)))

;; Get Topics items by category
(defn get-topics-by-category [category topics-data]
  (filter #(= (last (:path %)) category) topics-data))

;; Pico.css HTML Templates
(defn pico-page-layout [page-title content title tagline footer source base-path]
  (str "<!DOCTYPE html>
<html lang=\"fr\" data-theme=\"light\">
<head>
  <meta charset=\"utf-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
  <title>" page-title " - " title "</title>

  <!-- Pico.css 2.0 -->
  <link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css\">

  <style>
  /* Custom styles */
  .container {
      max-width: 1200px;
      margin: 0 auto;
      padding: 0 1rem;
  }

  header.site-header {
      padding: 1rem 0;
      background-color: #f8f9fa;
      margin-bottom: 2rem;
  }

  footer {
      margin-top: 3rem;
      padding: 2rem 0;
      background-color: #f8f9fa;
  }

  .category-card {
      height: 100%;
      display: flex;
      flex-direction: column;
  }

  .category-card > a {
      flex-grow: 1;
      display: flex;
      flex-direction: column;
      padding: 1.5rem;
      text-decoration: none;
      color: inherit;
      border: 1px solid #dee2e6;
      border-radius: 0.5rem;
      background-color: white;
      transition: transform 0.2s, box-shadow 0.2s;
  }

  .category-card > a:hover {
      transform: translateY(-5px);
      box-shadow: 0 10px 20px rgba(0, 0, 0, 0.1);
  }

  .grid {
      display: grid;
      grid-template-columns: repeat(auto-fill, minmax(300px, 1fr));
      gap: 2rem;
  }

  details {
      margin-bottom: 1rem;
      border: 1px solid #dee2e6;
      border-radius: 0.5rem;
      padding: 1rem;
  }

  details summary {
      cursor: pointer;
      font-weight: bold;
      padding: 0.5rem 0;
  }

  details[open] summary {
      margin-bottom: 1rem;
  }

  .back-link {
      display: inline-flex;
      align-items: center;
      margin-bottom: 1rem;
  }

  .back-link::before {
      content: '←';
      margin-right: 0.5rem;
  }

  .search-form {
      margin-bottom: 3rem;
  }

  .search-container {
      display: flex;
      gap: 0.5rem;
  }

  .search-container input[type=\"search\"] {
      flex-grow: 1;
  }

  .alert {
      padding:       1rem;
      border-radius: 0.5rem;
      margin-bottom: 1rem;
  }

  .alert-info {
      background-color: #e7f5ff;
      border:           1px solid #a5d8ff;
      color:            #1971c2;
  }

  .alert-error {
      background-color: #ffe3e3;
      border:           1px solid #ffa8a8;
      color:            #e03131;
  }
  </style>
  </head>
  <body>
  <header class=\"site-header\">
  <div class=\"container\">
  <div>
  <h1><a href=\"" (with-base-path "/" base-path) "\">" title "</a></h1>
  <p>" tagline "</p>
  </div>
  </div>
  </header>

  <main class=\"container\">
  " content "
  </main>

  <footer>
  <div class=\"container\">
  <p>" footer "</p>
  <p>Voir <a target=\"new\" href=\"" source "\">la source des questions et réponses</a></p>
  </div>
  </footer>

  <script>
  // Enable details/summary accordion behavior if needed
  document.addEventListener('DOMContentLoaded', function() {
                                                            // You can add JavaScript here if needed
                                                            });
  </script>
  </body>
  </html>"))

(defn home-content [topics-data base-path]
  (str "<div>
  <form action=\"" (with-base-path "/search" base-path) "\" method=\"get\" class=\"search-form\" role=\"search\">
  <input placeholder=\"Rechercher dans les sujets...\" type=\"search\" id=\"search-input\" name=\"q\">
  <button type=\"submit\">Rechercher</button>
  </form>
  <div class=\"grid\">"
       (str/join "\n"
                 (for [category (get-categories topics-data)]
                   (str "<div class=\"category-card\">
  <a href=\"" (with-base-path "/category" base-path) "?name=" (safe-url-encode category) "\">
  <h3>" category "</h3>
  <p>" (count (get-topics-by-category category topics-data)) " questions</p>
  </a>
  </div>")))
       "</div>
  </div>"))

(defn category-content [category-name category-topics base-path]
  (str "<div>
  <a href=\"" (with-base-path "/" base-path) "\" class=\"back-link\">Retour à l'accueil</a>
  <h2>" category-name "</h2>

  <div>"
       (str/join "\n"
                 (for [item category-topics]
                   (str "<details>
  <summary>" (:title item) "</summary>
  <div>" (:content item) "</div>
  </details>")))
       "</div>
  </div>"))

(defn search-content [query results base-path]
  (str "<div>
  <a href=\"" (with-base-path "/" base-path) "\" class=\"back-link\">Retour à l'accueil</a>
  <h2>Résultats de recherche</h2>
  <p>Résultats pour \"" query "\" (" (count results) ") :</p>

  <div>"
       (if (empty? results)
         "<div class=\"alert alert-info\">
  <h3>Aucun résultat</h3>
  <p>Aucun résultat ne correspond à votre recherche. Essayez avec d'autres termes.</p>
  </div>"
         (str "<div>"
              (str/join "\n"
                        (for [item results]
                          (str "<details>
  <summary>" (:title item) "</summary>
  <div>" (:content item) "</div>
  </details>")))
              "</div>"))
       "</div>
  </div>"))

(defn topics-content [item base-path]
  (str "<div>
  <a href=\"javascript:history.back()\" class=\"back-link\">Retour</a>
  <article>
  <h2>" (:title item) "</h2>
  <div>
  " (:content item) "
  </div>
  <p>
  Catégorie : <a href=\"" (with-base-path "/category" base-path) "?name=" (safe-url-encode (last (:path item))) "\">" (last (:path item)) "</a>
  </p>
  </article>
  </div>"))

(defn not-found-content [base-path]
  (str "<div>
  <a href=\"" (with-base-path "/" base-path) "\" class=\"back-link\">Retour à l'accueil</a>
  <h2>Contenu introuvable</h2>
  <div class=\"alert alert-error\">
  <h3>L'article demandé n'existe pas</h3>
  <p>Vérifiez l'URL ou effectuez une nouvelle recherche.</p>
  </div>
  </div>"))

(defn error-content [base-path]
  (str "<div>
  <a href=\"" (with-base-path "/" base-path) "\" class=\"back-link\">Retour à l'accueil</a>
  <h2>Page non trouvée</h2>
  <div class=\"alert alert-error\">
  <h3>La page demandée n'existe pas</h3>
  <p>Vérifiez l'URL ou retournez à l'accueil.</p>
  </div>
  </div>"))

;; Function to extract path without base path
(defn strip-base-path [uri base-path]
  (let [base-len (count base-path)]
    (if (and (seq base-path)
             (str/starts-with? uri base-path))
      (let [path (subs uri base-len)]
        (if (str/starts-with? path "/")
          path
          (str "/" path)))
      uri)))

;; Parse query string with improved safety
(defn parse-query-string [query-string]
  (when query-string
    (try
      (into {}
            (for [pair (str/split query-string #"&")]
              (let [[k v] (str/split pair #"=" 2)]  ;; Limit to 2 parts
                [(keyword (safe-url-decode k))
                 (safe-url-decode (or v ""))])))  ;; Handle missing values
      (catch Exception e
        (log/error "Error parsing query string:" (.getMessage e))
        {}))))

;; Create app function with topics-data and settings as parameters
(defn create-app [topics-data settings]
  (fn [{:keys [request-method uri query-string]}]
    (let [path   (strip-base-path uri (:base-path settings))
          params (parse-query-string query-string)]

      (case [request-method path]
        [:get "/"]
        {:status  200
         :headers {"Content-Type" "text/html; charset=utf-8"}
         :body    (pico-page-layout
                   "Accueil"
                   (home-content topics-data (:base-path settings))
                   (:title settings)
                   (:tagline settings)
                   (:footer settings)
                   (:source settings)
                   (:base-path settings))}

        [:get "/robots.txt"]
        {:status  200
         :headers {"Content-Type" "text/plain"}
         :body    "User-agent: *\nAllow: /\n"}

        [:get "/category"]
        (let [category-name   (:name params)
              category-topics (get-topics-by-category category-name topics-data)]
          {:status  200
           :headers {"Content-Type" "text/html; charset=utf-8"}
           :body    (pico-page-layout
                     (str "Catégorie : " category-name)
                     (category-content category-name category-topics (:base-path settings))
                     (:title settings)
                     (:tagline settings)
                     (:footer settings)
                     (:source settings)
                     (:base-path settings))})

        [:get "/search"]
        (let [query   (:q params)
              results (search-topics query topics-data)]
          {:status  200
           :headers {"Content-Type" "text/html; charset=utf-8"}
           :body    (pico-page-layout
                     (str "Résultats pour : " query)
                     (search-content query results (:base-path settings))
                     (:title settings)
                     (:tagline settings)
                     (:footer settings)
                     (:source settings)
                     (:base-path settings))})

        [:get "/topics"]
        (let [id   (:id params)
              item (first (filter #(= (:title %) id) topics-data))]
          (if item
            {:status  200
             :headers {"Content-Type" "text/html; charset=utf-8"}
             :body    (pico-page-layout
                       (:title item)
                       (topics-content item (:base-path settings))
                       (:title settings)
                       (:tagline settings)
                       (:footer settings)
                       (:source settings)
                       (:base-path settings))}
            {:status  404
             :headers {"Content-Type" "text/html; charset=utf-8"}
             :body    (pico-page-layout
                       "404"
                       (not-found-content (:base-path settings))
                       (:title settings)
                       (:tagline settings)
                       (:footer settings)
                       (:source settings)
                       (:base-path settings))}))

        ;; Default route - 404
        {:status  404
         :headers {"Content-Type" "text/html; charset=utf-8"}
         :body    (pico-page-layout
                   "Page non trouvée"
                   (error-content (:base-path settings))
                   (:title settings)
                   (:tagline settings)
                   (:footer settings)
                   (:source settings)
                   (:base-path settings))}))))

;; Show help
(defn show-help []
  (println "Usage: topics [options]")
  (println "\nOptions:")
  (println (cli/format-opts {:spec cli-options}))
  (System/exit 0))

;; Main function
(defn -main []
  (try
    ;; Parse command line arguments with babashka.cli
    (let [opts            (cli/parse-opts *command-line-args* {:spec cli-options})
          ;; Extract default values from cli-options for any missing settings
          defaults        (into {} (map (fn [[k v]] [k (:default v)]) cli-options))
          parsed-settings (merge defaults opts)]
      ;; Show help if requested
      (when (:help parsed-settings) (show-help))
      ;; Configure Timbre logging
      (log/merge-config! {:min-level (:log-level opts)})
      ;; Load Topics data
      (let [topics-data (load-topics-data (:topics parsed-settings))]
        ;; Start the server
        (log/info (str "Starting server at http://localhost:" (:port parsed-settings)))
        (if (empty? (:base-path parsed-settings))
          (log/info "Running at root path /")
          (log/info "Running at base path:" (:base-path parsed-settings)))
        (log/info "Site title:" (:title parsed-settings))
        (log/info "Site tagline:" (:tagline parsed-settings))
        (log/info "Topics source:" (:source parsed-settings))
        (server/run-server (create-app topics-data parsed-settings)
                           {:port (:port parsed-settings)})
        (log/info "Server started. Press Ctrl+C to stop.")
        @(promise)))
    (catch Exception e
      (log/error "ERROR:" (.getMessage e))
      (.printStackTrace e)
      (System/exit 1))))

;; Start the server
(-main)

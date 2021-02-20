#!/usr/bin/env bb
(ns mybbt.main
  (:require
   [clojure.java.shell :as sh]
   [clojure.java.io :as io]
   [clojure.repl :refer :all]
   [clojure.core :refer :all]
   [clojure.tools.cli :refer [parse-opts]]
   [babashka.process :refer [$ check process]]
   [clj-yaml.core :as yaml]
   [clojure.string :as str]))

(def pwd (str (System/getProperty "user.dir") "/"))

;; * data
(def reverse-proxies {:haproxy
                      {:image "haproxy:2.3.4-alpine"
                       :hostname "haproxy"
                       :volumes
                       ["/home/rgrau/workspace/dev-scripts/stacks/reverse-proxies/haproxy/config/:/usr/local/etc/haproxy/:ro"
                        "/home/rgrau/workspace/dev-scripts/stacks/reverse-proxies/haproxy/log:/dev/log"]
                       :networks ["d" "dp"]
                       :ports ["8080:80"]
                       :depends_on ["metabase"]
                       }
                      :envoy
                      {:image "envoyproxy/envoy-alpine:v1.17.0"
                       :hostname "envoy"
                       :volumes
                       ["/home/rgrau/workspace/dev-scripts/stacks/reverse-proxies/envoy/config/envoy.yaml:/etc/envoy/envoy.yaml"
                        "/home/rgrau/workspace/dev-scripts/stacks/reverse-proxies/envoy/logs:/var/log"]
                       :networks ["d" "dp"]
                       :ports ["8080:80"]
                       :depends_on ["metabase"]
                       }
                      :nginx
                      {:image "nginx:1.19.6-alpine"
                       :hostname "nginx"
                       :volumes
                       ["/home/rgrau/workspace/dev-scripts/stacks/reverse-proxies/nginx/nginx.conf:/etc/nginx/conf.d/default.conf"]
                       :networks ["d" "dp"]
                       :ports ["8080:80"]
                       :depends_on ["metabase"]}})

(def databases {:mysql57
                {:image "circleci/mysql:5.7.23"
                 :user "root"
                 :volumes ["/home/rgrau/.mba/home/:/root/"]
                 :restart "on-failure"
                 :stdin_open true
                 :tty true
                 :networks ["d"]
                 :labels {"com.metabase.d" true}}

                :mongodb
                {:image "circleci/mongo:4.0"
                 :user "root"
                 :volumes ["/home/rgrau/.mba/home/:/root/"]
                 :restart "on-failure"
                 :stdin_open true
                 :tty true
                 :networks ["d"]
                 :labels {"com.metabase.d" true}}

                :mariadb-latest
                {:image "mariadb:latest"
                 :volumes ["/home/rgrau/.mba/home/:/root/"]
                 :environment
                 {:MYSQL_DATABASE "metabase_test"
                  :MYSQL_USER "root"
                  :MYSQL_ALLOW_EMPTY_PASSWORD "yes"
                  :MBA_CLI "mysql --user=root --database=metabase_test"}
                 :restart "on-failure"
                 :stdin_open true
                 :tty true
                 :networks ["d"]
                 :labels {"com.metabase.d" true}}

                :presto
                {:image "metabase/presto-mb-ci"
                 :volumes ["/home/rgrau/.mba/home/:/root/"]
                 :restart "on-failure"
                 :stdin_open true
                 :tty true
                 :networks ["d"]
                 :labels {"com.metabase.d" true}}

                :sparksql
                {:image "metabase/spark:2.1.1"
                 :volumes ["/home/rgrau/.mba/home/:/root/"]
                 :restart "on-failure"
                 :stdin_open true
                 :tty true
                 :networks ["d"]
                 :labels {"com.metabase.d" true}}

                :sqlserver
                {:image "mcr.microsoft.com/mssql/server:2017-latest"
                 :environment
                 {:ACCEPT_EULA "Y"
                  :SA_PASSWORD "P@ssw0rd"}
                 :volumes ["/home/rgrau/.mba/home/:/root/"]
                 :restart "on-failure"
                 :stdin_open true
                 :tty true
                 :networks ["d"]
                 :labels {"com.metabase.d" true}}

                :vertica
                {:image "sumitchawla/vertica"
                 :environment
                 {:ACCEPT_EULA "Y"
                  :SA_PASSWORD "P@ssw0rd"}
                 :volumes ["/home/rgrau/.mba/home/:/root/"]
                 :restart "on-failure"
                 :stdin_open true
                 :tty true
                 :networks ["d"]
                 :labels {"com.metabase.d" true}}

                :postgres
                {:image "postgres:12"
                 :user "root"
                 :volumes ["/home/rgrau/.mba/home/:/root/"
                           "/home/rgrau/workspace/mba/resources/postgres/docker-entrypoint-initdb.d/:/docker-entrypoint-initdb.d/"]
                 :environment
                 {:POSTGRES_USER "metauser"
                  :POSTGRES_PASSWORD "metapass"
                  :POSTGRES_DB "metabase"
                  :POSTGRES_MULTIPLE_DATABASES "metabase,metabase_test,harbormaster_dev,harbormaster_test"
                  :POSTGRES_HOST_AUTH_METHOD "trust"
                  :MBA_CLI "psql -U metauser -d metabase"
                  :MBA_SEED "psql -U metauser -d metabase -f /root/seed_clean.sql >/dev/null"
                  :MBA_DUMP "pg_dump -U metauser metabase --clean >/root/seed_clean.sql"}
                :restart "on-failure"
                 :stdin_open true
                 :tty true
                 :networks ["d"]
                 :labels {"com.metabase.d" true}}

                :mysql
                {:image "circleci/mysql:5.7.23"
                 :environment
                 {:user "root"
                  :database "circle_test"}
                 :restart "on-failure"
                 :stdin_open true
                 :tty true
                 :networks ["d"]
                 :labels {"com.metabase.d" true}}

                :h2
                {:image "oscarfonts/h2"
                 :ports ["1521" "81"]
                 :volumes ["h2vol:/opt/h2-data/"]
                 :environment
                 {:MBA_CLI "bash"}
                 }})

(def docker-compose {:version "3.5"
                     :networks {:d {} :dp {}}
                     :volumes { :h2vol {}}
                     :services
                     {:maildev
                      {:image "maildev/maildev"
                       ;; :ports ["1080:80", "1025:25"]
                       :ports ["80", "25"]
                       :labels {"com.metabase.d" true}}

                      :metabase
                      {:build {:context (str pwd ".devcontainer/")
                               :dockerfile "Dockerfile"}

                       :working_dir "/app/source"
                       :volumes ["/home/rgrau/.mba/home/:/root/"
                                 (str (System/getProperty "user.dir") ":/app/source/")
                                 "h2vol:/app/source/metabase-h2-db/"]
                       :environment
                       {:MBA_CLI "lein run h2"
                        :MB_DB_FILE "/app/source/metabase-h2-db/metabase.db"
                        }
                       :tty "True"
                       :stdin_open "True"
                       :restart "on-failure"
                       :command "tail -f /dev/null"
                       :ports ["3000" "8080" "7888"]
                       :networks ["d" "dp"]
                       :labels {"com.metabase.d" true}}}})

(def all-dbs {:postgres "jdbc:postgresql://postgres:5432/metabase?user=metauser&password=metapass"
              :mariadb-latest "jdbc:mysql://mariadb-latest:3306/metabase_test?user=root"
              :mysql57 "jdbc:mysql://mysql57:3306/metabase_test?user=root"})

;; * docker-compose

(def my-temp-file (java.io.File/createTempFile "docker-compose-d-" ".yml"))

(defn docker-compose-yml [docker-compose]
  (yaml/generate-string docker-compose :dumper-options {:flow-style :block}))

(defn docker-compose-yml-file! [docker-compose-yml]
  (spit my-temp-file docker-compose-yml))


(defn- assemble-app-db
  [config app-db]
  (-> config
      (assoc-in [:services :metabase :environment :MB_DB_CONNECTION_URI] (app-db all-dbs))
      (assoc-in [:services app-db] (app-db databases))))

(defn- prepare-dc [opts]
  (let [app-db (keyword (:app-db opts))
        data-db (keyword (:data-db opts))
        proxy (keyword (:proxy opts))
        publish (:publish opts)]
    (-> (cond-> (assemble-app-db docker-compose app-db)

          ;; data-db
          (not (nil? (:data-db opts)))
          (assoc-in [:services data-db] (data-db databases))

          ;; network
          (not (nil? (:network opts)))
          (assoc-in [:networks :d] {:name (:network opts)})

          ;; dev / release
          (not (.exists (io/file (str (System/getProperty "user.dir") "/app.json"))))
          (->
           (assoc-in [:services :metabase :image]
                     (str "metabase/metabase"
                          (and (:enterprise opts) "-enterprise"))) ; not very cool entanglement
           (update-in [:services :metabase] dissoc :command))

          ;; CE / EE
          (not (nil? (:enterprise opts)))
          (update-in [:services :metabase :environment]
                     assoc
                     :ENABLE_ENTERPRISE_EDITION "true"
                     :HAS_ENTERPRISE_TOKEN "true"
                     :ENTERPRISE_TOKEN  "ENV ENT_TOKEN"
                     :MB_EDITION "ee")

          ;; proxy?
          proxy
          (update-in [:services] assoc proxy (proxy reverse-proxies))

          publish
          (assoc-in [:services :metabase :ports]
                    ["3000:3000" "8080:8080" "7888:7888"]))

        docker-compose-yml
        docker-compose-yml-file!)))

;;; * Tasks

(defmulti task first)

(defmethod task :default
  [[cmd opts args]]
  (prepare-dc opts)
  (process `["docker-compose" "-f" ~(.getPath my-temp-file) ~@args]
           {:out :inherit :err :inherit})
  ;; (-> ^{:out :inherit :err :inherit}
  ;;     ($ docker-compose -f ~my-temp-file ~(name cmd)))
  nil)

(defmacro with-filter
  "Still not useful now, but it will be.
  https://redhatnordicssa.github.io/shell-scripting-using-clojure-and-babashka"
  [command & forms]
  `(let [sh#  (or (System/getenv "SHELL") "sh")
         pb#  (doto (ProcessBuilder. [sh# "-c" ~command])
                (.redirectError
                 (ProcessBuilder$Redirect/to (io/file "/dev/tty"))))
         p#   (.start pb#)
         in#  (io/reader (.getInputStream p#))
         out# (io/writer (.getOutputStream p#))]
     (binding [*out* out#]
       (try ~@forms (.close out#) (catch Exception e#)))
     (take-while identity (repeatedly #(.readLine in#)))))

(defn- exec-into
  [container & cmds]
  (-> (ProcessBuilder. `["docker-compose" "-f" ~(.getPath my-temp-file) "exec" ~container "sh" "-l" "-i" "-c" ~@cmds])
      (.inheritIO)
      (.start)
      (.waitFor)))


(defn- exec-to
  ;; try with $ foo=hola mba exec-to echo foo
  [container & cmds]
  (-> (ProcessBuilder. `["sh" "-l" "-i" "-c" "env"])
      (.inheritIO)
      (.start)
      (.waitFor)))

(defmethod task :exec-to
  [[_ opts]]
  (prepare-dc opts)
  (exec-to "metabase" "bash"))

(defmethod task :shell
  [[_ opts]]
  (prepare-dc opts)
  (exec-into "metabase" "sh" "-l" "-c" "-i" "bash"))

(defmethod task :go
  ;; it is difficult to open the files from host but send things
  ;; through cider-connect through docker because the cider tells
  ;; emacs to open things in /root/.m2/.... but in localhost, they are
  ;; in /home/rgrau/.m2... . crap
  [[_ opts]]
  (prepare-dc opts)
  ;; "lein update-in :dependencies conj \[nrepl/nrepl\ \"0.8.3\"\] -- update-in :plugins conj \[cider/cider-nrepl\ \"0.25.8\"\] -- repl :headless :host 0.0.0.0 :port 7888"

  ;; lein update-in :dependencies conj \[nrepl/nrepl\ \"0.8.3\"\] -- update-in :plugins conj \[refactor-nrepl\ \"2.5.1\"\] -- update-in :plugins conj \[cider/cider-nrepl\ \"0.25.8\"\] -- repl :headless :host localhost ;
  (exec-into "metabase" "echo" "lein" "update-in" ":dependencies" "conj" "[nrepl/nrepl \"0.8.3\"]" "--"
             "update-in" ":plugins" "conj" "[refactor-nrepl \"2.5.1\"]" "--"
             "update-in" ":plugins" "conj" "[cider/cider-nrepl \"0.25.8\"]" "--"
             "repl" ":headless" ":host" "0.0.0.0" ":port" "7888"))

(defmethod task :dbconsole
  ;; EACH possible container should add an env var MBA_CLI that will
  ;; be ran by this command to open a shell.
  [[_ opts]]
  (prepare-dc opts)
  (let [app-db (name (:app-db opts))]
    (exec-into app-db "sh" "-l" "-i" "-c" "$MBA_CLI")))

(defmethod task :install-dep
  [[_ opts]]
  (prepare-dc opts)
  (-> ^{:out :inherit :err :inherit}
      ($ docker-compose -f ~my-temp-file exec metabase apt update))
  nil)


(defmethod task :help
  [summary]
  (let [next-piece [["##  " " ## " " ## " "####" " #  " "####" "####"]
                    [" ## " " ## " "##  " "    " "### " "#   " "   #"]]
        p1 (rand-int (count (first next-piece)))
        r1 (get (first next-piece) p1)
        r2 (get (second next-piece) p1)
        p2 (rand-int (count (first next-piece)))
        n1 (get (first next-piece) p2)
        n2 (get (second next-piece) p2)]
    ;; (println "HELP ME, BYZANTINE MUSICAL SYMBOL SYNAGMA META STAVROU! 𝀫")
    (println "            MetaBaseAssembler: ")
    (println "")
    (println "              *          * ")
    (println "              *          * ")
    (println "              *  " r1 "  * ")
    (println "              *  " r2 "  * ")
    (println "              *          * ")
    (println "              *          * ")
    (println "              *          * ")
    (println "              *          * ")
    (println "              *          * ")
    (println "              *         #* ")
    (println "  Next:       *         #* ")
    (println "              *   # ##  #* ")
    (println "  " n1 "      *  #####  #* ")
    (println "  " n2 "      ************ ")
    (println "")
    (println (second summary))
    (println "")
    (println "Emacs config:")
    (prn (setq cider-path-translations '(("/app/source" . "~/workspace/metabase")
                                         ("/root/.m2/" . "~/.mba/home/.m2/"))))


    ))

;; * CLI

(def cli-options
  "https://github.com/clojure/tools.cli#example-usage"
  [["-E" "--enterprise" "Enterprise edition"]
   ["-h" "--help" "HALP!"]
   ["-P" "--publish" "publish ports"]
   ;; ["-pp" "--port PORT" "Port number"
   ;;  :multi true
   ;;  :default ["3000" "8080" "7888"]
   ;;  ;:validate [#(re-find #"\d+:\d+")]   ;
   ;;  :update-fn conj;; (fn [acc x]
   ;;             ;;   (conj
   ;;             ;;    (remove #{(second (re-find #"^(\d+):\d+$" x))} acc)
   ;;             ;;    x))
   ;;  ]
   ["-p" "--prefix PREFIX" "Prefix of docker-compose run" :default "d"]
   ["-n" "--network NETWORK" "network name" :default nil]
   ["-t" "--tag TAG" "metabase/metabase:v0.37.9  or path-to-source"
    :default nil
    :validate [#(or (.exists (clojure.java.io/file %))
                    (re-find #"\w+/\w+:?.*" %))]]

   [nil "--proxy proxy-type" "use reverse proxy"
    :default nil
    :parse-fn (comp keyword str/lower-case)
    :validate [#{:nginx :envoy :haproxy}]]
   ["-d" "--app-db APP-DB"
    :default "h2"
    :parse-fn (comp keyword str/lower-case)
    :validate [#{:h2 :postgres :postgresql :mysql :mariadb-latest}]]
   ["-D" "--data-db DATA-DB"
    :default nil
    :parse-fn (comp keyword str/lower-case)
    :validate [#{:postgres :postgresql :mysql :mongo :mariadb-latest :vertica} ]]])

(defn -main
  "fubar"
  [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)
        [cmd & rest] arguments]
    (when (or (= cmd "help")
              (:help options))
      (task [:help summary])
      (System/exit 0))
    (when (seq errors)
      (println errors)
      (System/exit 1))
    (task [(keyword (or cmd :help)) options arguments])
    nil))

(apply -main *command-line-args*)

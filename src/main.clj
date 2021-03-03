#!/usr/bin/env bb
(ns mybbt.main
  (:require
   ;; [babashka.fs :as fs]
   [clojure.java.shell :as sh]
   [clojure.java.io :as io]
   [clojure.repl :refer :all]
   [clojure.core :refer :all]
   [clojure.tools.cli :refer [parse-opts]]
   [babashka.process :refer [$ check process]]
   [clj-yaml.core :as yaml]
   [clojure.string :as str]))

;; * ಠ_ಠ
(def pwd (str (System/getProperty "user.dir") "/"))
(def home (str (System/getProperty "user.home") "/"))
(def mba-config (str home ".mba/"))
(def mba-home (str mba-config ".mba-home/"))

;; (def resources
;;   (-> *file*
;;        io/file
;;        .getParentFile
;;        .getParent
;;        (io/file "resources")
;;        str))

(def resources
  (str (str/trim-newline (:out
                          (sh/sh "dirname"
                                 (str/trim-newline
                                  (:out (sh/sh "realpath" (str *file*)))))))  "/../resources/"))


;; * data
(def reverse-proxies {:haproxy
                      {:image "haproxy:2.3.4-alpine"
                       :hostname "haproxy"
                       :volumes
                       [(str resources "/stacks/reverse-proxies/haproxy/config/:/usr/local/etc/haproxy/:ro")
                        (str resources "/stacks/reverse-proxies/haproxy/log:/dev/log")]
                       :networks ["d" "dp"]
                       :ports ["8080:80"]
                       :depends_on ["metabase"]
                       }
                      :envoy
                      {:image "envoyproxy/envoy-alpine:v1.17.0"
                       :hostname "envoy"
                       :volumes
                       [(str resources "/stacks/reverse-proxies/envoy/config/envoy.yaml:/etc/envoy/envoy.yaml")
                        (str resources "/stacks/reverse-proxies/envoy/logs:/var/log")]
                       :networks ["d" "dp"]
                       :ports ["8080:80"]
                       :depends_on ["metabase"]
                       }
                      :nginx
                      {:image "nginx:1.19.6-alpine"
                       :hostname "nginx"
                       :volumes
                       [(str resources "/stacks/reverse-proxies/nginx/nginx.conf:/etc/nginx/conf.d/default.conf")]
                       :networks ["d" "dp"]
                       :ports ["8080:80"]
                       :depends_on ["metabase"]}})

(def databases {:mysql57
                {:image "circleci/mysql:5.7.23"
                 :user "root"
                 :volumes [(str mba-home ":/root/")]
                 :restart "on-failure"
                 :stdin_open true
                 :tty true
                 :networks ["d"]
                 :labels {"com.metabase.d" true}}

                :mongodb
                {:image "circleci/mongo:4.0"
                 :user "root"
                 :volumes [(str mba-home ":/root/")]
                 :restart "on-failure"
                 :stdin_open true
                 :tty true
                 :networks ["d"]
                 :labels {"com.metabase.d" true}}

                :mariadb-latest
                {:image "mariadb:latest"
                 :volumes [(str mba-home ":/root/")]
                 :environment
                 {:MYSQL_DATABASE "metabase_test"
                  :MYSQL_USER "root"
                  :MYSQL_ALLOW_EMPTY_PASSWORD "yes"
                  :MBA_DB_CLI "mysql --user=root --database=metabase_test"}
                 :restart "on-failure"
                 :stdin_open true
                 :tty true
                 :networks ["d"]
                 :labels {"com.metabase.d" true}}

                :presto
                {:image "metabase/presto-mb-ci"
                 :volumes [(str mba-home ":/root/")]
                 :restart "on-failure"
                 :stdin_open true
                 :tty true
                 :networks ["d"]
                 :labels {"com.metabase.d" true}}

                :sparksql
                {:image "metabase/spark:2.1.1"
                 :volumes [(str mba-home ":/root/")]
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
                 :volumes [(str mba-home ":/root/")]
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
                 :volumes [(str mba-home ":/root/")]
                 :restart "on-failure"
                 :stdin_open true
                 :tty true
                 :networks ["d"]
                 :labels {"com.metabase.d" true}}

                :postgres
                {:image "postgres:12"
                 :user "root"
                 :volumes [(str mba-home ":/root/")
                           (str resources "/postgres/docker-entrypoint-initdb.d/:/docker-entrypoint-initdb.d/")]
                 :environment
                 {:POSTGRES_USER "metauser"
                  :POSTGRES_PASSWORD "metapass"
                  :POSTGRES_DB "metauser"
                  :POSTGRES_MULTIPLE_DATABASES "metabase,metabase_test,harbormaster_dev,harbormaster_test"
                  :POSTGRES_HOST_AUTH_METHOD "trust"
                  :MBA_DB_CLI "psql -U metauser -d metabase"
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
                 :command "tail -f /dev/null" ; if we let the image
                                              ; run normally it locks
                                              ; the db file!!! wtf
                 :environment
                 {:MBA_DB_CLI "bash"}
                 }})

(def docker-compose {:version "3.5"
                     :networks {:d {} :dp {}}
                     :volumes { :h2vol {}}
                     :services
                     {:maildev
                      {:image "maildev/maildev"
                       ;; :ports ["1080:80", "1025:25"]
                       :ports ["80", "25"]
                       :labels {"com.metabase.d" true}
                       :networks ["d"]
                       }
                      :metabase
                      {:depends_on ["maildev"]
                       :working_dir "/app/source"
                       :volumes [(str mba-home ":/root/") ; home
                                 "h2vol:/app/source/metabase-h2-db/"
                                 ;; XXX: if you change this, 00-restore-env.sh
                                 ;; will be the one from the docker
                                 ;; image, which sets java 15 as the
                                 ;; default one, and mb doesn't start
                                 ;; then.
                                 ;; (str resources "/base/profile.sh:/etc/profile.d/99-profile.sh")
                                 (str resources "/base/profile.sh:/etc/profile.d/00-restore-env.sh")]
                       :environment
                       {:ENABLE_ENTERPRISE_EDITION "true"
                        :HAS_ENTERPRISE_TOKEN "true"
                        :ENTERPRISE_TOKEN  "ENV ENT_TOKEN"
                        :MB_EDITION "ee"
                        ;; :JAVA_OPTS "-Dlog4j.configurationFile=file:///metabase.db/log4j2.xml"
                        :MBA_PREFIX "MB"
                        :MBA_DB_CLI "lein run h2"
                        :MB_DB_FILE "/app/source/metabase-h2-db/metabase.db"
                        :MB_EMAIL_SMTP_HOST "maildev"
                        :MB_EMAIL_SMTP_PORT "25"
                        :MB_ENABLE_TEST_ENDPOINTS "true"
                        :MBA_CLI "lein update-in :dependencies conj \\[nrepl/nrepl\\ \\\"0.8.3\\\"\\] -- update-in :plugins conj \\[refactor-nrepl\\ \\\"2.5.1\\\"\\] -- update-in :plugins conj \\[cider/cider-nrepl\\ \\\"0.25.8\\\"\\] -- repl-ee :headless :host 0.0.0.0  :port 7888"
                        :MBA_YARN_BUILD "yarn && NODE_ENV=hot yarn webpack-dev-server --progress --host 0.0.0.0"
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
  (let [[name version] (str/split app-db #":")
        kw-name (keyword name)]
    (-> config
        (assoc-in [:services :metabase :environment :MB_DB_CONNECTION_URI] (kw-name all-dbs))
        (assoc-in [:services kw-name] (kw-name databases))
        (update-in [:services :metabase :depends_on] conj name)
        (cond-> version (assoc-in [:services kw-name :image] app-db)))))

(defn- prepare-dc [opts]
  (let [app-db (:app-db opts)
        data-db (keyword (:data-db opts))
        proxy (keyword (:proxy opts))
        publish (:publish opts)
        prefix (:prefix opts)
        [protocol metabase] (:mb opts)
        metabase (str/replace metabase #"~/" (str (System/getProperty "user.home") "/"))]

    (-> (cond-> (assemble-app-db docker-compose app-db)

          ;; data-db
          (not (nil? (:data-db opts)))
          (assoc-in [:services data-db] (data-db databases))

          ;; network
          (not (nil? (:network opts)))
          (assoc-in [:networks :d] {:name (:network opts)})

          ;; dev / release
          ;; (not (.exists (io/file (str (System/getProperty "user.dir") "/app.json"))))

          (#{"docker" "dockerhub"} protocol)
          (->
           (assoc-in [:services :metabase :image] metabase)
           (update-in [:services :metabase] dissoc :command)
           (update-in [:services :metabase] dissoc :build))

          (#{"gh" "github"} protocol)
          (throw "NYI")

          (#{"file"} protocol)
          (->
           (update-in [:services :metabase :volumes] conj
                      (str (-> metabase io/file .getCanonicalPath ) ":/app/source/"))
           (assoc-in [:services :metabase :build]
                     {:context (str (-> metabase io/file .getCanonicalPath ) "/.devcontainer/") ; ?! maybe unify to
                      :dockerfile "Dockerfile"}))

          ;; proxy?
          proxy
          (update-in [:services] assoc proxy (proxy reverse-proxies))

          (and publish (not proxy))
          (assoc-in [:services :metabase :ports]
                    ["3000:3000" "8080:8080" "7888:7888"])

          ;;prefix
          ;;(update :services #(update-values % assoc-in [:environment :prefix] prefix))
          )

        docker-compose-yml
        docker-compose-yml-file!)))

;;; * Tasks
(defmulti task first)

(defmethod task :default
  [[cmd opts args]]
  (prepare-dc opts)
  (-> (ProcessBuilder. `["docker-compose" "-f" ~(.getPath my-temp-file) ~@args])
      (.inheritIO)
      (.start)
      (.waitFor))

  ;; (process `["docker-compose" "-f" ~(.getPath my-temp-file) ~@args]
  ;;          {:out :inherit :err :inherit})
  ;; (-> ^{:out :inherit :err :inherit}
  ;;     ($ docker-compose -f ~my-temp-file ~(name cmd)))
  )

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
  ;; https://github.com/babashka/babashka/blob/master/examples/process_builder.clj
  ;; https://github.com/babashka/babashka/issues/299
  ;; https://book.babashka.org/#child_processes
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

(defmethod task :up
  [[_ opts args]]
  (prepare-dc opts)
  (-> (ProcessBuilder. `["docker-compose" "-f" ~(.getPath my-temp-file) "up" "-d"])
      (.inheritIO)
      (.start)
      (.waitFor))
  (println args opts))


(defmethod task :shell
  [[_ opts]]
  (prepare-dc opts)
  ;; for now we specialcase this
  ;; because (exec-into "metabase" "bash" "-l" "-i") wouldn't trigger
  ;; the .profile.

  ;; should add "-l" , but it makes java 15 the default java, which we
  ;; don't want because it fails to run
  (-> (ProcessBuilder. `["docker-compose" "-f" ~(.getPath my-temp-file) "exec" "metabase" "bash" "-l" "-i"])
      (.inheritIO)
      (.inheritIO)
      (.start)
      (.waitFor)))

(defmethod task :go
  [[_ opts]]
  (prepare-dc opts)
  (exec-into "metabase" "eval $MBA_CLI" ))

(defmethod task :dbconsole
  ;; EACH possible db container should add an env var MBA_DB_CLI that
  ;; will be ran by this command to open a shell.
  [[_ opts]]
  (let [app-db (:app-db opts)
        app-db-service (first (str/split app-db #":"))]
    (prepare-dc opts)
    (exec-into app-db-service "$MBA_DB_CLI")))

(defmethod task :graph
  [[_ opts]]
  (prepare-dc opts)

  (let [opener
        (if (re-find #"Linux"
                     (System/getProperty "os.name"))
          "xdg-open"
          "open")]
    (sh/sh  "docker" "run" "--rm" "--name" "dcv" "-v" "/tmp/:/input"
            "pmsipilot/docker-compose-viz" "render" "-m" "image"
            (str/replace (.getPath my-temp-file) "/tmp/" "") "--force")
    (sh/sh opener "/tmp/docker-compose.png")))

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
        n-pieces (count (first next-piece))
        p1 (rand-int n-pieces)
        p2 (rand-int n-pieces)
        [r1 r2] (map #(get (% next-piece) p1) [first second])
        [n1 n2] (map #(get (% next-piece) p2) [first second])]
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
    (println "Summary:")
    (println summary)
    (println "Usage:")
    (println "mba up")
    (println "mba go")
    (println "mba dbconsole")
    (println "firefox $(mba port maildev 80)")
    (println "mba logs -- -f -t maildev")
    (println "mba logs -- -f -t postgres")
    (println "")
    (println "Emacs config:")
    (prn '(setq cider-path-translations '(("/app/source" . "~/workspace/metabase")
                                          ("/root/.m2/" . "~/.mba-home/.m2/"))))))

;; * CLI

(def cli-options
  "https://github.com/clojure/tools.cli#example-usage"
  [["-M" "--mb METABASE"
    :default (if (.exists (io/file (str pwd "/app.json")))
               ["file" "./"]
               ["docker" "metabase/metabase"])
    :parse-fn (fn [arg]
                (let [[prot where] (str/split arg #":" 2)]
                  (if (seq where) [prot where] ["file" prot])))
    :validate [#(re-find #"(dockerhub|docker|github|git|gh|file)" (first %)) second]]
   ["-h" "--help" "HALP!"]
   ["-P" "--publish PUBLISH"
    "publish ports"
    :default true
    :validate [#{"true" "false"}]]
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
    :validate [#(or (.exists (io/file %))
                    (re-find #"\w+/\w+:?.*" %))]]

   [nil "--proxy proxy-type" "use reverse proxy"
    :default nil
    :parse-fn (comp keyword str/lower-case)
    :validate [#{:nginx :envoy :haproxy}]]
   ["-d" "--app-db APP-DB"
    :default "h2"
    :parse-fn str/lower-case
    :validate [#(#{:h2 :postgres :postgresql :mysql :mariadb-latest} (keyword (re-find #"^[^:]*" %)))]]
   ["-D" "--data-db DATA-DB"
    :default nil
    :parse-fn (comp keyword str/lower-case)
    :validate [#{:postgres :postgresql :mysql :mongo :mariadb-latest :vertica} ]]])



(defn copy-file [source-path dest-path]
  (io/copy (io/file source-path) (io/file dest-path)))

(defn- ensure-dirs []
  (when-not (.exists (io/file mba-home))
    (.mkdirs (io/file mba-home))
    (copy-file (str resources "/home/.bashrc") (str mba-home "/.bashrc"))
    (copy-file (str resources "/home/.bash_profile") (str mba-home "/.bash_profile"))))

(defn -main
  "fubar"
  [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)
        [cmd & rest] arguments]
    (when (or (= cmd "help")
              (:help options))
      (prn options)
      (task [:help summary])
      (System/exit 0))
    (when (seq errors)
      (println errors)
      (System/exit 1))
    (ensure-dirs)
    (task [(keyword (or cmd :help)) options arguments])
    nil))

(apply -main *command-line-args*)

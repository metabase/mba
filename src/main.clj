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
                       :ports ["8082:80"]
                       :depends_on ["metabase"]
                       }
                      :nginx
                      {:image "nginx:1.19.6-alpine"
                       :hostname "nginx"
                       :volumes
                       ["/home/rgrau/workspace/dev-scripts/stacks/reverse-proxies/nginx/nginx.conf:/etc/nginx/conf.d/default.conf"]
                       :networks ["d" "dp"]
                       :ports ["8081:80"]
                       :depends_on ["metabase"]
                       }})

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
                  :MYSQL_ALLOW_EMPTY_PASSWORD "yes"}
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
                 :volumes ["/home/rgrau/.mba/home/:/root/"]
                 :environment
                 {:POSTGRES_USER "rgrau"
                  :POSTGRES_PASSWORD "rgrau"
                  :POSTGRES_DB "rgrau"
                  :POSTGRES_HOST_AUTH_METHOD "trust"}
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
                })

(def docker-compose {:version "3.5"
                     :networks {:d {} :dp {}}
                     :services
                     {:maildev
                      {:image "maildev/maildev"
                       ;; :ports ["1080:80", "1025:25"]
                       :ports ["80", "25"]}

                      :metabase
                      {:build {:context "/home/rgrau/workspace/metabase/.devcontainer/"
                               :dockerfile "Dockerfile"}

                       :working_dir "/app/source"
                       :volumes ["/home/rgrau/.mba/home/:/root/"
                                 "/home/rgrau/.mba/.m2:/root/.m2"
                                 "/home/rgrau/.mba/node_modules/:/root/node_modules/"
                                 (str (System/getProperty "user.dir") ":/app/source/")]

                       :environment {}
                       :tty "True"
                       :stdin_open "True"
                       :restart "on-failure"
                       :command "tail -f /dev/null"
                       :ports ["3000:3000" "8080:8080" "7888:7888"]
                       :networks ["d" "dp"]
                       :labels {"com.metabase.d" true}}}
                     })

(def all-dbs {:postgres "jdbc:postgresql://postgres:5432/rgrau?user=rgrau&password=rgrau"
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
  (if (= :h2 app-db)
    (update-in config [:services :metabase :volumes] conj (str (System/getProperty "user.dir") "/metabase.db.mv.db"))
    (-> config
        (assoc-in [:services :metabase :environment :MB_DB_CONNECTION_URI] (app-db all-dbs))
        (assoc-in [:services app-db] (app-db databases)))))

(defn- prepare-dc [opts]
  (let [app-db (keyword (:app-db opts))
        data-db (keyword (:data-db opts))
        proxy (keyword (:proxy opts))]
    (-> (cond-> (assemble-app-db docker-compose app-db)

          (not (nil? (:data-db opts)))
          (assoc-in [:services data-db] (data-db databases))

          (not (nil? (:network opts)))
          (assoc-in [:networks :d] {:name (:network opts)})

          (not (.exists (io/file (str (System/getProperty "user.dir") "/app.json"))))
          (assoc-in [:services :metabase :image] "metabase/metabase")

          (not (nil? (:enterprise opts)))
          (update-in [:services :metabase :environment]
                     assoc
                     :ENABLE_ENTERPRISE_EDITION "true"
                     :HAS_ENTERPRISE_TOKEN "true"
                     :ENTERPRISE_TOKEN  "ENV ENT_TOKEN"
                     :MB_EDITION "ee")

          (not (nil? proxy))
          (update-in [:services] assoc proxy (proxy reverse-proxies)))

        docker-compose-yml
        docker-compose-yml-file!)))

;;; * Tasks

(defmulti task first)

(defmethod task :default
  [[cmd opts]]
  (prepare-dc opts)
  (-> ^{:out :inherit :err :inherit}
      ($ docker-compose -f ~my-temp-file ~(name cmd)))
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
  (-> (ProcessBuilder. `["docker-compose" "-f" ~(.getPath my-temp-file) "exec" ~container ~@cmds])
      (.inheritIO)
      (.start)
      (.waitFor)))

(defmethod task :shell
  [[_ opts]]
  (prepare-dc opts)
  (exec-into "metabase" "bash"))

(defmethod task :dbconsole
  [[_ opts]]
  (prepare-dc opts)
  (exec-into "postgres" "psql" "-U" "rgrau"))

(defmethod task :install-dep
  [[_ opts]]
  (prepare-dc opts)
  (-> ^{:out :inherit :err :inherit}
      ($ docker-compose -f ~my-temp-file exec metabase apt update))
  nil)

(defmethod task :help
  [args]
  (println "HELP ME!"))

;; * CLI

(def cli-options
  [["-E" "--enterprise" "Enterprise edition"]
   ["-pp" "--port PORT" "Port number"
    :default 80
    :parse-fn #(Integer/parseInt %)
    :validate [#(< % 0x10000) "Must be between 0 and 65536"]]
   ["-p" "--prefix PREFIX" "Prefix of docker-compose run" :default "d"]
   ["-n" "--network NETWORK" "network name" :default nil]
   [nil "--proxy proxy-type" "use reverse proxy" :default nil]
   ["-d" "--app-db APP-DB"
    :default "h2"
    :parse-fn (comp keyword str/lower-case)
    :validate [#{:h2 :postgres :postgresql :mysql :mariadb-latest}]]
   ["-D" "--data-db DATA-DB"
    :default "postgres"
    :parse-fn (comp keyword str/lower-case)
    :validate [#{:postgres :postgresql :mysql :mongo :mariadb-latest}]]])

(defn -main
  "fubar"
  [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)
        [cmd & rest] arguments]
    (when (seq errors)
      (println errors)
      (System/exit 1))
    (task [(keyword (or cmd :help)) options])
    nil))

(apply -main *command-line-args*)

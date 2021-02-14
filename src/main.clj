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

(def databases {:imagemysql57
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
  )

(def docker-compose {:version "3.5"
                     :networks {:d nil}
                     :services
                     {:maildev
                      {:image "maildev/maildev"
                       ;; :ports ["1080:80", "1025:25"]
                       :ports ["80", "25"]}

                      :metabase
                      {:image "clojure"
                       :working_dir "/app/source"
                       :volumes ["/home/rgrau/.mba/home/:/root/"
                                 "/home/rgrau/.mba/.m2:/root/.m2"
                                 "/home/rgrau/.mba/node_modules/:/root/node_modules/"
                                 (str (System/getProperty "user.dir") ":/app/source/")]

                       :environment {}
                       :tty true
                       :stdin_open true
                       :restart "on-failure"
                       :command "tail -f /dev/null"
                       :ports ["3000:3000" "8080:8080" "7888:7888"]
                       :networks ["d"]
                       :labels {"com.metabase.d" true}}}
                     })

(def cli-options
  [["-pp" "--port PORT" "Port number"
    :default 80
    :parse-fn #(Integer/parseInt %)
    :validate [#(< % 0x10000) "Must be between 0 and 65536"]]
   ["-p" "--prefix PREFIX" "Prefix of docker-compose run" :default "d"]
   ["-n" "--network NETWORK" "network name" :default nil]
   ["-d" "--app-db APP-DB"
    :default "h2"
    :parse-fn (comp keyword str/lower-case)
    :validate [#{:h2 :postgres :postgresql :mysql :mariadb-latest}]]
   ["-D" "--data-db APP-DB"
    :default "postgresql"
    :parse-fn (comp keyword str/lower-case)
    :validate [#{:postgres :postgresql :mysql :mongo}]]])

(defmulti task first)

(def my-temp-file (java.io.File/createTempFile "docker-compose-d-" ".yml"))

(defn massage-format [dc]
  ;; (map #(dissoc dc [ % :x-extra]) dc)
  dc
  )

(defn docker-compose-yml [docker-compose]
  (yaml/generate-string (massage-format docker-compose) :dumper-options {:flow-style :block}))

(defn docker-compose-yml-file!
  [docker-compose-yml]
  (spit my-temp-file docker-compose-yml)
  ;; (with-open [file (clojure.java.io/writer my-temp-file)]
  ;;  (binding [*out* file]
  ;;    (println
  ;;     (yaml/generate-string docker-compose :dumper-options {:flow-style :block}))))
  )

(def all-dbs {:postgres "jdbc:postgresql://postgres:5432/rgrau?user=rgrau&password=rgrau"
              :mariadb-latest "jdbc:mysql://mariadb-latest:3306/metabase_test?user=root"})

(defmethod task :up
  [[cmd opts]]
  (let [app-db (keyword (:app-db opts))]
    (-> (cond-> docker-compose
            (not= :h2 app-db)
          (assoc-in [:services :metabase :environment :MB_DB_CONNECTION_URI]
                    (app-db all-dbs))
          (not= :h2 app-db)
          (assoc-in [:services app-db] (app-db databases))

          (not (nil? (:network opts)))
          (assoc-in [:networks :d] {:name (:network opts)}))
        docker-compose-yml
        docker-compose-yml-file!))
  (->
      ($ ^{:out :inherit :err :inherit} docker-compose -f ~my-temp-file config))
  nil)

(defmethod task :down
  [args]
  (docker-compose-yml-file! )
  (-> ^{:out :inherit :err :inherit}
      ($ docker-compose -f ~my-temp-file down)))

(defmethod task :dbconsole
  []
  1)

(defmethod task :help
  [args]
  (println "helpa!"))

(defn -main
  "fubar"
  [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)
        [cmd & rest] arguments]
    (when (seq errors)
      (println errors)
      (System/exit 1))
    (task [(keyword (or cmd :help)) options])))

(apply -main *command-line-args*)

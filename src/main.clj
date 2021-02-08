#!/usr/bin/env bb
(ns mybbt.main
  (:require
   [clojure.java.shell :as sh]
   [clojure.java.io :as io]
   [clojure.repl :refer :all]
   [clojure.core :refer :all]
   [clojure.tools.cli :refer [parse-opts]]
   [clj-yaml.core :as yaml]
   [clojure.string :as str]))

(def docker-compose {:version "3.5"
                     :services
                     {:testing
                      {:image "ubuntu"
                       :user "root"
                       :command "tail -f /dev/null"
                       :networks ["d"]
                       :labels {"com.metabase.d" true}}
                      :postgres
                      {:image "postgres:12"
                       :user "root"
                       :volumes ["/home/rgrau/.gojira-kongs/.gojira-home/:/root/"]
                       :environment
                       {:POSTGRES_USER "root"
                        :POSTGRES_DB "metabase"
                        :POSTGRES_HOST_AUTH_METHOD "trust"}
                       :restart "on-failure"
                       :stdin_open true
                       :tty true
                       :networks ["d"]
                       :labels {"com.metabase.d" true}}
                      :maildev
                      {:image "maildev/maildev"
                       ;; :ports ["1080:80", "1025:25"]
                       :ports ["80", "25"]}

                      :metabase
                      {:image "clojure"
                       :volumes ["/home/rgrau/.gojira-kongs/.gojira-home/:/root/"
                                 "/home/rgrau/.m2:/root/.m2"
                                 "/home/rgrau/node_modules/:/root/node_modules"
                                 (str (System/getProperty "user.dir") ":/app/source/")]

                       :environment {:MB_CONNECTION_URI ;; "jdbc:mysql:mysql//mysql:3306/circle_test?user=root"
                                     "jdbc:mysql:mysql//mysql:3306/circle_test?user=root"
                                     }
                       :tty true
                       :stdin_open true
                       :restart "on-failure"
                       :command "tail -f /dev/null"
                       :ports ["3000:3000"]
                       :networks ["d"]
                       :labels {"com.metabase.d" true}
                       }

                      :mysql
                      {:image "circleci/mysql:5.7.23"
                       :environment
                       {:user "root"
                        :database "circle_test"}
                       :restart "on-failure"
                       :stdin_open true
                       :tty true
                       :networks ["d"]
                       :labels {"com.metabase.d" true}}}
                     :networks {:d nil}
                     })

(def cli-options
  [["-pp" "--port PORT" "Port number"
    :default 80
    :parse-fn #(Integer/parseInt %)
    :validate [#(< % 0x10000) "Must be between 0 and 65536"]]
   ["-p" "--prefix PREFIX" "Prefix of docker-compose run"
    :default "d"]
   ["-d" "--app-db APP-DB"
    :default "h2"
    :parse-fn (comp keyword str/lower-case)
    :validate [#{:h2 :postgres :postgresql :mysql}]]
   ["-D" "--data-db APP-DB"
    :default "postgresql"
    :parse-fn (comp keyword str/lower-case)
    :validate [#{:postgres :postgresql :mysql :mongo}]]
   ])

(defmulti task first)

(def my-temp-file (java.io.File/createTempFile "docker-compose-d-" ".yml"))

(defn create-docker-compose-yml!
  []
  (spit my-temp-file
        (yaml/generate-string docker-compose :dumper-options {:flow-style :block}))
  ;; (with-open [file (clojure.java.io/writer my-temp-file)]
  ;;  (binding [*out* file]
  ;;    (println
  ;;     (yaml/generate-string docker-compose :dumper-options {:flow-style :block}))))
  )

(defmethod task :up
  [args]
  (create-docker-compose-yml!)
  ;; (sh/sh (format "docker-compose -f %s" my-temp-file))
  ;; (println "up" args)
  )

(defmethod task :down
  [args]
  42)

(defn -main
  "fubar"
  [& args]
  (let [opts (parse-opts args cli-options)
        [cmd & rest] (:arguments opts) ]
    ;; (task [(keyword cmd)])
    )
  (println (yaml/generate-string docker-compose :dumper-options {:flow-style :block}))
  )

(apply -main *command-line-args*)

(defproject clj-test1 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [incanter "1.5.7"]]

  :java-source-paths ["D:\\clj-testovi\\src\\lib"]
   :main clj-test1.montecarlo-simulation
   :aot [clj-test1.montecarlo-simulation]
  )


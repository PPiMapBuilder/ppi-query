(defproject ppi-query "0.1.0-SNAPSHOT"
  :description "PPiMapBuilder network generation library"
  :url "https://github.com/Soaring-Outliers/ppi-query"
  :license {:name "GNU GENERAL PUBLIC LICENSE Version 3"
            :url "http://www.gnu.org/licenses/gpl.txt"}

  :repositories {"EBI-IntAct" "http://www.ebi.ac.uk/intact/maven/nexus/content/groups/public"}

  ; Avoid random exception when using clojure.spec.test/check with clojure.test
  :monkeypatch-clojure-test false

  :dependencies [[org.clojure/clojure "1.9.0-alpha15"]
                 [org.clojure/spec.alpha "0.1.123"]
                 [org.clojure/java.data "0.1.1"]
                 [org.clojure/data.zip "0.1.2"]
                 [com.taoensso/nippy "2.13.0"]

                 ; HTTP client
                 [clj-http "2.3.0"]

                 ; PSICQUIC
                 [psidev.psi.mi/psimitab "1.8.4"]
                 [org.hupo.psi.mi.psicquic/psicquic-simple-client "1.3.3"]]

  :pom-plugins [[com.theoryinpractise/clojure-maven-plugin "1.8.1"
                 ;; this section is optional, values have the same syntax as pom-addition
                 {:configuration [:sourceDirectories [:sourceDirectory "src"]]
                  :extensions "true"
                  :executions ([:execution [:id "compile"]
                                [:goals ([:goal "compile"])]
                                [:phase "compile"]])}]]

  :plugins [[lein-auto "0.1.3"]]

  :profiles {:dev {:aot :all
                   :dependencies [[proto-repl "0.3.1"]
                                  [proto-repl-charts "0.3.2"]
                                  [aprint "0.1.3"]
                                  [io.aviso/pretty "0.1.33"]
                                  [org.clojure/test.check "0.9.0"]]}})

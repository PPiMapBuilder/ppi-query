(defproject ppi-query "0.1.0-SNAPSHOT"
  :description "PPiMapBuilder network generation library"
  :url "https://github.com/Soaring-Outliers/ppi-query"
  :license {:name "GNU GENERAL PUBLIC LICENSE Version 3"
            :url "http://www.gnu.org/licenses/gpl.txt"}

  :repositories {"EBI-IntAct" "https://www.ebi.ac.uk/intact/maven/nexus/content/groups/public"}

  ; Avoid random exception when using clojure.spec.test/check with clojure.test
  :monkeypatch-clojure-test false

  :dependencies [[org.clojure/clojure "1.9.0-alpha15"]
                 [org.clojure/spec.alpha "0.1.123"]
                 [org.clojure/java.data "0.1.1"]
                 [org.clojure/data.zip "0.1.2"]
                 [org.clojure/data.json "0.2.6"]
                 [com.taoensso/nippy "2.13.0"]

                 ; HTTP client
                 [clj-http "2.3.0"]

                 ; Command line arguments
                 [org.clojure/tools.cli "0.3.5"]

                 ; PSICQUIC
                 [psidev.psi.mi/psimitab "1.8.4"]
                 [org.hupo.psi.mi.psicquic/psicquic-simple-client "1.3.3"]

                 ; Web server
                 [compojure "1.6.0"] ; Routes
                 [hiccup "1.0.5"] ; Create HTML
                 [ring/ring-core "1.6.3"] ; Server web
                 [ring/ring-devel "1.6.3"]
                 [ring/ring-jetty-adapter "1.6.3"]]

  :dev-dependencies
                [[lein-run "1.0.0"]]

  :pom-plugins [[com.theoryinpractise/clojure-maven-plugin "1.8.1"
                 ;; this section is optional, values have the same syntax as pom-addition
                 {:configuration [:sourceDirectories [:sourceDirectory "src"]]
                  :extensions "true"
                  :executions ([:execution [:id "compile"]
                                [:goals ([:goal "compile"])]
                                [:phase "compile"]])}]]

  :plugins [[lein-auto "0.1.3"]]

  :jvm-opts ["-Dfile.encoding=utf-8"]

  :profiles {:dev {:aot :all
                   :dependencies [[proto-repl "0.3.1"]
                                  [proto-repl-charts "0.3.2"]
                                  [aprint "0.1.3"]
                                  [io.aviso/pretty "0.1.33"]
                                  [org.clojure/test.check "0.9.0"]]}}

  :main ppi-query.network-launch
  :aot [ppi-query.network-launch])

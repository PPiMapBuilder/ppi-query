(defproject ppi-query "0.1.0-SNAPSHOT"
  :description "PPiMapBuilder network generation library"
  :url "https://github.com/Soaring-Outliers/ppi-query"
  :license {:name "GNU GENERAL PUBLIC LICENSE Version 3"
            :url "http://www.gnu.org/licenses/gpl.txt"}

  :repositories {"EBI-IntAct" "http://www.ebi.ac.uk/intact/maven/nexus/content/groups/public"}

  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [org.clojure/java.data "0.1.1"]

                 ; HTTP client
                 [clj-http "2.3.0"]

                 ; PSICQUIC
                 [psidev.psi.mi/psimitab "1.8.4"]
                 [org.hupo.psi.mi.psicquic/psicquic-simple-client "1.3.3"]
                 [org.hupo.psi.mi.psicquic/psicquic-registry-client "1.1.0"]]

  :plugins [[lein-auto "0.1.3"]]
  :profiles {:dev
              {:dependencies
                [[proto-repl "0.3.1"]
                 [proto-repl-charts "0.3.2"]
                 [org.clojure/test.check "0.9.0"]]}})

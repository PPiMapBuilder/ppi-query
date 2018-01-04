(ns ppi-query.interaction.display
  (:require [clojure.pprint :refer :all]))

(defn display-map [disp1 m]
  (dorun
    (map (fn [[k v]]
            (if (and v
                    (not (= false v))
                    (or (and (coll? v) (not-empty v))
                        (and (not (coll? v)) (some? v))))
               (do (disp1 k)
                   (pprint v))))
         m)))

(defn display-tree [depth tree]
  (println "display-tree" tree)
  (cond (map? tree)
        (dorun
          (map (fn [[k v]]
                  (if (not-empty v)
                     (do (println "==" k "==")
                         (display-tree (inc depth) v))
                     (println [k v])))
               tree))
        (coll? tree)
        (dorun (map (partial display-tree (inc depth)) tree))
        (some? tree)
        (pprint tree)))

(defn display-interactor [interactor]
  (display-map #(println "--" % "--") interactor))

(defn display-interaction [interaction]
  (println "#### Interaction ####")
  (display-map #(println "==" % "==") (dissoc interaction :interactorA :interactorB))
  (println "## :interactorA ##")
  (display-interactor (interaction :interactorA))
  (println "## :interactorB ##")
  (display-interactor (interaction :interactorB)))

(defn display-prot-orths-multi-interaction
  [{pa :protein-a
    opa :ortholog-protein-a
    pb :protein-b
    opb :ortholog-protein-b
    oints :original-interactions
    :as pomi}]
  (println "Multi Interaction")
  (do (println "Reference organism interaction:")
      (println (:organism pa))
      (println (:uniprotid pa) "<->" (:uniprotid pb)))
  (if opa
      (do (println "Ortholog interaction:")
          (println (:organism opa))
          (println (:uniprotid opa) "<->" (:uniprotid opb))
          (println (:ortholog-score opa) "<->" (:ortholog-score opb))))
  (dorun (map display-interaction oints)))

(defn display-prot-orths-multi-interactions
  [pomis]
  (dorun (map display-prot-orths-multi-interaction pomis)))

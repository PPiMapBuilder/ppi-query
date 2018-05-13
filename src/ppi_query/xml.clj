(ns ppi-query.xml
  (:require [clojure.spec.alpha :as s]
            [ppi-query.spec :as ps]))

; Specs for the clojure representation of xml (clojure.xml)
(s/def ::tag keyword?)
(s/def ::attrs (s/nilable map?))
(s/def ::content (s/nilable (s/coll-of ::document)))
(s/def ::node (s/keys :req-un [::tag ::attrs ::content]))
(s/def ::document (s/or :text string? :node ::node :nil nil?))

(defmacro node [& {:keys [tag attrs content]}]
  "Creates an xml node spec describing its tag, attributes and content."
  `(ps/map-spec :tag ~(or tag `::tag)
                :attrs ~(or attrs `::attrs)
                :content ~(or content `::content)))

(defmacro text-node [tag]
  "Spec for xml node containing only text."
  `(node :tag #{~tag} :content (s/nilable (s/coll-of string?))))

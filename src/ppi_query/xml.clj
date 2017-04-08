(ns ppi-query.xml
  (:require [clojure.spec :as s]))

; Specs for the clojure representation of xml (clojure.xml)
(s/def ::tag keyword?)
(s/def ::attrs (s/nilable map?))
(s/def ::content (s/nilable (s/coll-of ::document)))
(s/def ::node (s/keys :req-un [::tag ::attrs ::content]))
(s/def ::document (s/or :text string? :node ::node :nil nil?))


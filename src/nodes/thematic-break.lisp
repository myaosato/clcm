(defpackage :clcm/nodes/thematic-break
  (:use :cl :clcm/node)
  (:export :thematic-break-node))
(in-package :clcm/nodes/thematic-break)

(defclass thematic-break-node (node)
  ())

(defmethod close!? ((node thematic-break-node) line)
  nil)

(defmethod add!? ((node thematic-break-node) line)
  nil)

(defmethod ->html ((node thematic-break-node))
  (format nil "<hr />~%"))

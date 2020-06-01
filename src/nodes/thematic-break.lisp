(defpackage :clcm/nodes/thematic-break
  (:use :cl
        :clcm/node)
  (:import-from :cl-ppcre
                :scan)
  (:export :thematic-break-node
           :is-thematic-break-line))
(in-package :clcm/nodes/thematic-break)

;;
(defclass thematic-break-node (node)
  ())

(defmethod close!? ((node thematic-break-node) line)
  nil)

(defmethod add!? ((node thematic-break-node) line)
  nil)

(defmethod ->html ((node thematic-break-node))
  (format nil "<hr />~%"))


;;
(defun is-thematic-break-line (line)
  (or (scan "^ {0,3}(?:\\*\\s*){3,}$" line)
      (scan "^ {0,3}(?:_\\s*){3,}$" line)
      (scan "^ {0,3}(?:-\\s*){3,}$" line)))

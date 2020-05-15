(defpackage :clcm/nodes/document
  (:use :cl :clcm/node)
  (:import-from :clcm/line
                :is-blank-line)
  (:import-from :clcm/nodes/paragraph
                :paragraph-node)
  (:export :document-node))
(in-package :clcm/nodes/document)

(defclass document-node (node)
  ())

(defmethod close!? ((node document-node) line)
  nil)

(defmethod add!? ((node document-node) line)
  (cond ((is-blank-line line)
         nil)
        (t
         (add-child node (make-instance 'paragraph-node))
         (add!? (last-child node) line))))

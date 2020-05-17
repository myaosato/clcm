(defpackage :clcm/nodes/document
  (:use :cl :clcm/node)
  (:import-from :cl-ppcre
                :scan)
  (:import-from :clcm/line
                :is-blank-line
                :is-thematic-break-line)
  (:import-from :clcm/nodes/thematic-break
                :thematic-break-node)
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
        ((is-thematic-break-line line)
         (add-child node (make-instance 'thematic-break-node :is-open nil)))
        (t
         (add-child node (make-instance 'paragraph-node))
         (add!? (last-child node) line))))

(defmethod ->html ((node document-node))
  (format nil "~{~A~}"
               (mapcar #'->html (children node))))

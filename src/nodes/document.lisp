(defpackage :clcm/nodes/document
  (:use :cl :clcm/node)
  (:import-from :cl-ppcre
                :scan)
  (:import-from :clcm/line
                :is-blank-line
                :is-thematic-break-line
                :is-atx-heading-line
                :is-indented-code-block-line)
  (:import-from :clcm/nodes/atx-heading
                :atx-heading-node)
  (:import-from :clcm/nodes/thematic-break
                :thematic-break-node)
  (:import-from :clcm/nodes/indented-code-block
                :indented-code-block-node)
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
        ((is-atx-heading-line line)
         (add-child node (make-instance 'atx-heading-node :is-open nil))
         (add!? (last-child node) line))
        ((is-thematic-break-line line)
         (add-child node (make-instance 'thematic-break-node :is-open nil)))
        ((is-indented-code-block-line line)
         (add-child node (make-instance 'indented-code-block-node))
         (add!? (last-child node) line))
        (t
         (add-child node (make-instance 'paragraph-node))
         (add!? (last-child node) line))))

(defmethod ->html ((node document-node))
  (format nil "~{~A~}"
          (mapcar #'->html (children node))))

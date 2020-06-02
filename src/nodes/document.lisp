(defpackage :clcm/nodes/document
  (:use :cl
        :clcm/node
        :clcm/nodes/thematic-break
        :clcm/nodes/atx-heading
        :clcm/nodes/indented-code-block
        :clcm/nodes/fenced-code-block
        :clcm/nodes/html-block
        :clcm/nodes/paragraph
        :clcm/nodes/block-quote)
  (:import-from :cl-ppcre
                :scan)
  (:import-from :clcm/line
                :is-blank-line
                :skip-blank-line?)
  (:import-from :clcm/nodes/block-quote-methods)
  (:export :document-node))
(in-package :clcm/nodes/document)

(defclass document-node (node)
  ())

(defmethod close!? ((node document-node) line)
  nil)

(defmethod add!? ((node document-node) line)
  (or (skip-blank-line? line)
      (attach-thematic-break!? node line)
      (attach-atx-heading!? node line)
      (attach-indented-code-block!? node line)
      (attach-fenced-code-block!? node line)
      (attach-html-block!? node line)
      (attach-block-quote!? node line)
      (attach-paragraph! node line)))

(defmethod ->html ((node document-node))
  (format nil "~{~A~}"
          (mapcar #'->html (children node))))

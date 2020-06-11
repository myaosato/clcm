(defpackage :clcm/nodes/document
  (:use :cl
        :clcm/line
        :clcm/node
        :clcm/nodes/thematic-break
        :clcm/nodes/atx-heading
        :clcm/nodes/indented-code-block
        :clcm/nodes/fenced-code-block
        :clcm/nodes/html-block
        :clcm/nodes/paragraph
        :clcm/nodes/block-quote
        :clcm/nodes/bullet-list
        :clcm/nodes/ordered-list)
  (:import-from :cl-ppcre
                :scan)
  (:import-from :clcm/nodes/block-quote-methods)
  (:import-from :clcm/nodes/bullet-list-methods)
  (:import-from :clcm/nodes/bullet-list-item-methods)
  (:import-from :clcm/nodes/ordered-list-methods)
  (:import-from :clcm/nodes/ordered-list-item-methods)
  (:export :document-node))
(in-package :clcm/nodes/document)

(defclass document-node (node)
  ())

;; close
(defmethod close!? ((node document-node) line offset)
  (let ((last-child (last-child node)))
    (when (and last-child (is-open last-child))
      (close!? last-child line offset))))

;; add
(defun _add!? (node line offset)
  (or (skip-blank-line? line)
      (attach-thematic-break!? node line offset)
      (attach-atx-heading!? node line offset)
      (attach-indented-code-block!? node line offset)
      (attach-fenced-code-block!? node line offset)
      (attach-html-block!? node line offset)
      (attach-block-quote!? node line offset)
      (attach-bullet-list!? node line offset)
      (attach-ordered-list!? node line offset)
      (attach-paragraph! node line)))

(defmethod add!? ((node document-node) line offset)
  (let ((last-child (last-child node)))
    (if (and last-child (is-open last-child))
        (add!? last-child line offset)
        (_add!? node line offset))))

;; html
(defmethod ->html ((node document-node))
  (format nil "~{~A~}"
          (mapcar #'->html (children node))))

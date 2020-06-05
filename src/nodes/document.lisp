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
        :clcm/nodes/block-quote)
  (:import-from :cl-ppcre
                :scan)
  (:import-from :clcm/nodes/block-quote-methods)
  (:export :document-node))
(in-package :clcm/nodes/document)

(defclass document-node (node)
  ())

;; close
(defmethod close!? ((node document-node) line)
  (let ((last-child (last-child node)))
    (when (and last-child (is-open last-child))
      (close!? last-child line))))

;; add
(defun _add!? (node line)
  (or (skip-blank-line? line)
      (attach-thematic-break!? node line)
      (attach-atx-heading!? node line)
      (attach-indented-code-block!? node line)
      (attach-fenced-code-block!? node line)
      (attach-html-block!? node line)
      (attach-block-quote!? node line)
      (attach-paragraph! node line)))

(defmethod add!? ((node document-node) line)
  (let ((last-child (last-child node)))
    (if (and last-child (is-open last-child))
        (add!? last-child line)
        (_add!? node line))))

;; html
(defmethod ->html ((node document-node))
  (format nil "~{~A~}"
          (mapcar #'->html (children node))))

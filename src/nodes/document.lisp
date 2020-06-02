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
  ;; outer utils
  (:import-from :cl-ppcre
                :scan)
  ;; inner utils
  (:import-from :clcm/line
                :is-blank-line)
  ;;
  (:import-from :clcm/nodes/block-quote-methods)
  (:export :document-node))
(in-package :clcm/nodes/document)

(defclass document-node (node)
  ())

(defmethod close!? ((node document-node) line)
  nil)

(defmethod add!? ((node document-node) line)
  (cond ((is-blank-line line)
         nil)
        ((attach-thematic-break!? node line))
        ((attach-atx-heading!? node line))
        ((attach-indented-code-block!? node line))
        ((or (is-backtick-fenced-code-block-line line)
             (is-tilde-fenced-code-block-line line))
         (add-child node (make-fenced-code-block-node line)))
        ((is-html-block-type-1-start-line line)
         (add-child node (make-instance 'html-block-node :block-type 1))
         (add!? (last-child node) line))
        ((is-html-block-type-2-start-line line)
         (add-child node (make-instance 'html-block-node :block-type 2))
         (add!? (last-child node) line))
        ((is-html-block-type-3-start-line line)
         (add-child node (make-instance 'html-block-node :block-type 3))
         (add!? (last-child node) line))
        ((is-html-block-type-4-start-line line)
         (add-child node (make-instance 'html-block-node :block-type 4))
         (add!? (last-child node) line))
        ((is-html-block-type-5-start-line line)
         (add-child node (make-instance 'html-block-node :block-type 5))
         (add!? (last-child node) line))
        ((is-html-block-type-6-start-line line)
         (add-child node (make-instance 'html-block-node :block-type 6))
         (add!? (last-child node) line))
        ((is-html-block-type-7-start-line line)
         (add-child node (make-instance 'html-block-node :block-type 7))
         (add!? (last-child node) line))
        ((is-block-quote-line line)
         (add-child node (make-instance 'block-quote-node))
         (add!? (last-child node) line))
        (t
         (add-child node (make-instance 'paragraph-node))
         (add!? (last-child node) line))))

(defmethod ->html ((node document-node))
  (format nil "~{~A~}"
          (mapcar #'->html (children node))))

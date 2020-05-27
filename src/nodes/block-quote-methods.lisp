(defpackage :clcm/nodes/block-quote-methods
  (:use :cl :clcm/node)
  (:import-from :cl-ppcre
                :scan-to-strings)
  (:import-from :clcm/nodes/block-quote
                :block-quote-node)
  (:import-from :clcm/line
                :is-blank-line
                :is-thematic-break-line
                :is-atx-heading-line
                :is-indented-code-block-line
                :is-backtick-fenced-code-block-line
                :is-tilde-fenced-code-block-line
                :is-html-block-type-1-start-line
                :is-html-block-type-2-start-line
                :is-html-block-type-3-start-line
                :is-html-block-type-4-start-line
                :is-html-block-type-5-start-line
                :is-html-block-type-6-start-line
                :is-html-block-type-7-start-line
                :is-block-quote-line)
  (:import-from :clcm/nodes/atx-heading
                :atx-heading-node)
  (:import-from :clcm/nodes/thematic-break
                :thematic-break-node)
  (:import-from :clcm/nodes/indented-code-block
                :indented-code-block-node)
  (:import-from :clcm/nodes/fenced-code-block
                :make-fenced-code-block-node)
  (:import-from :clcm/nodes/html-block
                :html-block-node)
  (:import-from :clcm/nodes/paragraph
                :paragraph-node)
  (:export :trim-block-quote-marker))
(in-package :clcm/nodes/block-quote-methods)

;; for paragraph in block quote
(defun close-paragraph-line (line)
  (or (is-blank-line line)
      (is-thematic-break-line line)
      (is-atx-heading-line line)
      (is-backtick-fenced-code-block-line line)
      (is-tilde-fenced-code-block-line line)
      (is-html-block-type-1-start-line line)
      (is-html-block-type-2-start-line line)
      (is-html-block-type-3-start-line line)
      (is-html-block-type-4-start-line line)
      (is-html-block-type-5-start-line line)
      (is-html-block-type-6-start-line line)))

(defmethod close!? ((node block-quote-node) line)
  (cond ((not (or (typep (last-child node) 'paragraph-node) (is-block-quote-line line)))
         (close-node node))
        ((and (typep (last-child node) 'paragraph-node) (close-paragraph-line line))
         (close-node (last-child node))
         (close-node node))))

(defun trim-block-quote-marker (line)
  (multiple-value-bind (has-marker content) (scan-to-strings "^ {0,3}> ?(.*)$" line)
    (if has-marker
        (aref content 0)
        line)))

(defmethod add!? ((node block-quote-node) line)
  (let ((line (trim-block-quote-marker line)))
    (cond ((is-blank-line line)
           nil)
          ((is-atx-heading-line line)
           (add-child node (make-instance 'atx-heading-node :is-open nil))
           (add!? (last-child node) line))
          ((is-thematic-break-line line)
           (add-child node (make-instance 'thematic-break-node :is-open nil)))
          ((or (is-backtick-fenced-code-block-line line)
               (is-tilde-fenced-code-block-line line))
           (add-child node (make-fenced-code-block-node line)))
          ((is-indented-code-block-line line)
           (add-child node (make-instance 'indented-code-block-node))
           (add!? (last-child node) line))
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
           (add!? (last-child node) line)))))

(defmethod ->html ((node block-quote-node))
  (format nil "<blockquote>~%~{~A~}</blockquote>~%"
          (mapcar #'->html (reverse (children node)))))

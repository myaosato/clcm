(defpackage :clcm/nodes/block-quote-methods
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
                :scan-to-strings)
  (:import-from :clcm/line
                :is-blank-line)
  (:export :trim-block-quote-marker))
(in-package :clcm/nodes/block-quote-methods)

;; for paragraph in block quote
(defun close-paragraph-line (line)
  (or (is-blank-line line)
      (is-thematic-break-line line)
      (is-atx-heading-line line)
      (is-backtick-fenced-code-block-line line)
      (is-tilde-fenced-code-block-line line)
      (is-html-block-line '(1 2 3 4 5 6) line)
      (is-block-quote-line line)))

(defun trim-block-quote-marker (line)
  (multiple-value-bind (has-marker content) (scan-to-strings "^ {0,3}> ?(.*)$" line)
    (if has-marker
        (aref content 0)
        line)))

(defun _close!? (node line)
  (cond ((not (or (typep (last-child node) 'paragraph-node) (is-block-quote-line line)))
         (close-node node))
        ((and (typep (last-child node) 'paragraph-node)
              (close-paragraph-line (trim-block-quote-marker line)))
         (close-node (last-child node))
         (unless (is-block-quote-line line)
           (close-node node)))))

(defmethod close!? ((node block-quote-node) line)
  node)

(defmethod close!? :around ((node block-quote-node) line)
  (_close!? node line)
  (when (and (typep (last-child node) 'node)
             (is-open (last-child node)))
    (close!? (last-child node) (trim-block-quote-marker line))))

(defun _add!? (node line)
  (let ((line (trim-block-quote-marker line)))
    (cond ((is-blank-line line)
           nil)
          ((attach-thematic-break!? node line))
          ((attach-atx-heading!? node line))
          ((attach-indented-code-block!? node line))
          ((attach-fenced-code-block!? node line))
          ((attach-html-block!? node line))
          ((is-block-quote-line line)
           (add-child node (make-instance 'block-quote-node))
           (add!? (last-child node) line))
          (t
           (add-child node (make-instance 'paragraph-node))
           (add!? (last-child node) line)))))

(defmethod add!? ((node block-quote-node) line)
  node)

(defmethod add!? :around ((node block-quote-node) line)
  (let ((last-child (last-child node)))
    (if (and last-child
             (typep last-child 'node)
             (is-open last-child))
        (add!? last-child (trim-block-quote-marker line))
        (_add!? node line))))

(defmethod ->html ((node block-quote-node))
  (format nil "<blockquote>~%~{~A~}</blockquote>~%"
          (mapcar #'->html (children node))))

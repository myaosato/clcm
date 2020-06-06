(defpackage :clcm/nodes/block-quote-methods
  (:use :cl
        :clcm/utils
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
                :scan-to-strings)
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
  (multiple-value-bind (has-marker content) (scan-to-strings "^ {0,3}>([ \\t]?)(\\t?)(.*)$" line)
    (cond ((and has-marker (string= (aref content 0) " ") (string= (aref content 1) *string-tab*))
           (concatenate 'string "  " (aref content 2)))
          ((and has-marker (string= (aref content 0) " ") (string= (aref content 1) ""))
           (aref content 2))
          ((and has-marker
                (string= (aref content 0) *string-tab*) (string= (aref content 1) *string-tab*))
           (concatenate 'string "      " (aref content 2)))
          ((and has-marker (string= (aref content 0) *string-tab*) (string= (aref content 1) ""))
           (concatenate 'string "  " (aref content 2)))
          (has-marker
           (aref content 2))
          (t line))))

;; close
(defun _close!? (node line)
  (cond ((not (or (typep (last-child node) 'paragraph-node) (is-block-quote-line line)))
         (close-node node))
        ((and (typep (last-child node) 'paragraph-node)
              (close-paragraph-line (trim-block-quote-marker line)))
         (close-node (last-child node))
         (unless (is-block-quote-line line)
           (close-node node)))))

(defmethod close!? ((node block-quote-node) line offset)
  (_close!? node line)
  (let ((last-child (last-child node)))
    (when (and last-child (is-open (last-child node)))
      (close!? last-child (trim-block-quote-marker line) offset))))

;; add
(defun _add!? (node line)
  (let ((trimed-line (trim-block-quote-marker line)))
    (or (skip-blank-line? trimed-line)
      (attach-thematic-break!? node trimed-line)
      (attach-atx-heading!? node trimed-line)
      (attach-indented-code-block!? node trimed-line)
      (attach-fenced-code-block!? node trimed-line)
      (attach-html-block!? node trimed-line)
      (attach-block-quote!? node trimed-line)
      (attach-paragraph! node trimed-line))))

(defmethod add!? ((node block-quote-node) line offset)
  (declare (ignore offset))
  (let ((last-child (last-child node)))
    (if (and last-child (is-open last-child))
        (add!? last-child (trim-block-quote-marker line) 0)
        (_add!? node line))))

;; ->html
(defmethod ->html ((node block-quote-node))
  (format nil "<blockquote>~%~{~A~}</blockquote>~%"
          (mapcar #'->html (children node))))

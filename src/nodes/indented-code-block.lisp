(defpackage :clcm/nodes/indented-code-block
  (:use :cl :clcm/node)
  (:import-from :cl-ppcre
                :scan
                :scan-to-strings)
  (:import-from :clcm/utils
                :->)
  (:import-from :clcm/line
                :is-indented-code-block-close-line
                :is-blank-line)
  (:export :indented-code-block-node))
(in-package :clcm/nodes/indented-code-block)

(defclass indented-code-block-node (node)
  ())

(defmethod close!? ((node indented-code-block-node) line)
  (if (is-indented-code-block-close-line line)
      (close-node node)))

(defmethod add!? ((node indented-code-block-node) line)
  (if (scan "^ {0,3}$" line)
      (add-child node "")
      (let ((content (-> (multiple-value-list (scan-to-strings "^(?: {0,3}\\t|    )(.*)$" line))
                          (cadr)
                          (aref 0))))
        (add-child node content))))

(defmethod ->html ((node indented-code-block-node))
  (let ((content (make-content node)))
  (format nil "<pre><code>~A</code></pre>~%" content)))

(defun make-content (node)
  (format nil "~{~A~%~}" (-> (children node)
                             (trim-left-blank-line)
                             (reverse)
                             (trim-left-blank-line))))

(defun trim-left-blank-line (list)
  (cond ((null list)
         list)
        ((is-blank-line (car list))
         (trim-left-blank-line (cdr list)))
        (t
         list)))
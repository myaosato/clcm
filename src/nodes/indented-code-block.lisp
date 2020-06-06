(defpackage :clcm/nodes/indented-code-block
  (:use :cl
        :clcm/utils
        :clcm/line
        :clcm/node)
  (:import-from :cl-ppcre
                :scan
                :scan-to-strings)
  (:export :indented-code-block-node
           :is-indented-code-block-line
           :attach-indented-code-block!?))
(in-package :clcm/nodes/indented-code-block)

(defclass indented-code-block-node (node)
  ())

;; close
(defmethod close!? ((node indented-code-block-node) line offset)
  (declare (ignore offset))
  (if (is-indented-code-block-close-line line)
      (close-node node)))

;; add
(defmethod add!? ((node indented-code-block-node) line offset)
  (declare (ignore offset))
  (if (scan "^ {0,3}$" line)
      (add-child node "")
      (let ((content (-> (multiple-value-list (scan-to-strings "^(?: {0,3}\\t|    )(.*)$" line))
                         (cadr)
                         (aref 0))))
        (add-child node content))))

;; ->html
(defmethod ->html ((node indented-code-block-node))
  (let ((content (make-content node)))
  (format nil "<pre><code>~A</code></pre>~%" content)))

(defun make-content (node)
  (format nil "~{~A~%~}" (-> (children node)
                             (reverse)
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

;;
(defun is-indented-code-block-line (line)
  (scan "^(?: {0,3}\\t|    )" line))

(defun is-indented-code-block-close-line (line)
  (scan "^ {0,3}\\S" line))

(defun attach-indented-code-block!? (node line)
  (when (is-indented-code-block-line line)
    (let ((child (make-instance 'indented-code-block-node)))
      (add-child node child)
      (add!? child line 0)
      child)))

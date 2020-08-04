(defpackage :clcm/nodes/indented-code-block
  (:use :cl
        :clcm/utils
        :clcm/line
        :clcm/inlines/inlines
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
  (if (is-indented-code-block-close-line line offset)
      (close-node node)))

;; add
(defmethod add!? ((node indented-code-block-node) line offset)
  (multiple-value-bind (indent content) (get-indented-depth-and-line line offset)
    (if (<= indent 3)
        (add-child node "")
        (add-child node (subseq content 4)))))

;; ->html
(defmethod ->html ((node indented-code-block-node))
  (let ((content (make-content node)))
  (format nil "<pre><code>~A</code></pre>~%" content)))

(defun make-content (node)
  (inlines->html* (trim-blank-line (children node)) :last-break t))

(defun trim-blank-line (list)
  (-> list
      (reverse)
      (trim-left-blank-line)
      (reverse)
      (trim-left-blank-line)))

(defun trim-left-blank-line (list)
  (cond ((null list)
         list)
        ((is-blank-line (car list))
         (trim-left-blank-line (cdr list)))
        (t
         list)))

;;
(defun is-indented-code-block-line (line offset)
  (let ((indent (get-indented-depth-of line offset)))
    (>= indent 4)))

(defun is-indented-code-block-close-line (line offset)
  (let ((indent (get-indented-depth-of line offset)))
    (and (not (is-blank-line line)) (< indent 3))))

(defun attach-indented-code-block!? (node line offset)
  (when (is-indented-code-block-line line offset)
    (let ((child (make-instance 'indented-code-block-node)))
      (add-child node child)
      (add!? child line offset)
      child)))

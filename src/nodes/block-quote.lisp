(defpackage :clcm/nodes/block-quote
  (:use :cl
        :clcm/line
        :clcm/node)
  (:import-from :cl-ppcre
                :scan)
  (:export :block-quote-node
           :is-block-quote-line
           :attach-block-quote!?))
(in-package :clcm/nodes/block-quote)

(defclass block-quote-node (node)
  ())

;; block quote
(defun is-block-quote-line (line offset)
  (multiple-value-bind (indent offset) (get-indented-depth-of line offset)
    (and (<= indent 3) (scan "^>" line :start indent))))

(defun attach-block-quote!? (node line offset)
  (when (is-block-quote-line line offset)
    (let ((child (make-instance 'block-quote-node)))
      (add-child node child)
      (add!? child line offset)
      child)))

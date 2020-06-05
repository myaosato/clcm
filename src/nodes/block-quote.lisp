(defpackage :clcm/nodes/block-quote
  (:use :cl :clcm/node)
  (:import-from :cl-ppcre
                :scan)
  (:export :block-quote-node
           :is-block-quote-line
           :attach-block-quote!?))
(in-package :clcm/nodes/block-quote)

(defclass block-quote-node (node)
  ())

;; block quote
(defvar *block-quote-marker*
  `(:sequence
    :start-anchor
    (:greedy-repetition 0 3 " ")
    ">"))

(defun is-block-quote-line (line)
  (scan *block-quote-marker* line))

(defun attach-block-quote!? (node line)
  (when (is-block-quote-line line)
    (let ((child (make-instance 'block-quote-node)))
      (add-child node child)
      (add!? child line 0)
      child)))

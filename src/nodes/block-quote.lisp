(defpackage :clcm/nodes/block-quote
  (:use :cl :clcm/node)
  (:import-from :cl-ppcre
                :scan)
  (:export :block-quote-node
           :is-block-quote-line))
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

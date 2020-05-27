(defpackage :clcm/nodes/block-quote
  (:use :cl :clcm/node)
  (:export :block-quote-node))
(in-package :clcm/nodes/block-quote)

(defclass block-quote-node (node)
  ())

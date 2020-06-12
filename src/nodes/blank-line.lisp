(defpackage :clcm/nodes/blank-line
  (:use :cl
        :clcm/line
        :clcm/node)
  (:export :blank-line-node))
(in-package :clcm/nodes/blank-line)

(defclass blank-line-node (node)
  ())

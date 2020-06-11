(defpackage :clcm/nodes/list
  (:use :cl
        :clcm/line
        :clcm/node)
  (:import-from :cl-ppcre
                :scan)
  (:export :list-node
           :marker
           :is-tight))
(in-package :clcm/nodes/list)

(defclass list-node (node)
  ((marker :accessor marker :initarg :marker)
   (is-tight :accessor is-tight :initarg :is-tight :initform t)))

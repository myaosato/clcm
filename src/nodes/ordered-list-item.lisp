(defpackage :clcm/nodes/ordered-list-item
  (:use :cl
        :clcm/utils
        :clcm/line
        :clcm/node
        :clcm/nodes/list-item)
  (:import-from :cl-ppcre
                :scan)
  (:export :ordered-list-item-node
           :offset
           :parent-is-tight
           :attach-ordered-list-item!))
(in-package :clcm/nodes/ordered-list-item)

(defclass ordered-list-item-node (list-item-node)
  ())

(defun attach-ordered-list-item! (node line offset)
  (attach-list-item! 'ordered-list-item-node node line offset))

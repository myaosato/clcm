(defpackage :clcm/nodes/bullet-list-item
  (:use :cl
        :clcm/utils
        :clcm/line
        :clcm/node
        :clcm/nodes/list-item)
  (:import-from :cl-ppcre
                :scan)
  (:export :bullet-list-item-node
           :offset
           :parent-is-tight
           :attach-bullet-list-item!))
(in-package :clcm/nodes/bullet-list-item)

(defclass bullet-list-item-node (list-item-node)
  ())

(defun attach-bullet-list-item! (node line offset)
  (attach-list-item! 'bullet-list-item-node node line offset))

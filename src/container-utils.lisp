(defpackage :clcm/container-utils
  (:use :cl
        :clcm/node
        :clcm/nodes/paragraph
        :clcm/nodes/block-quote
        :clcm/nodes/bullet-list
        :clcm/nodes/bullet-list-item)
  (:export :has-paragraph-as-last))
(in-package :clcm/container-utils)

;;
(defun has-paragraph-as-last (node)
  (let ((last-child (last-child node)))
    (cond ((and (typep last-child 'paragraph-node) (is-open last-child))
           t)
          ((or (typep last-child 'block-quote-node)
               (typep last-child 'bullet-list-node)
               (typep last-child 'bullet-list-item-node))
           (has-paragraph-as-last last-child))
          (t nil))))

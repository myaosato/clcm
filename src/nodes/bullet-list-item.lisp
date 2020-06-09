(defpackage :clcm/nodes/bullet-list-item
  (:use :cl
        :clcm/line
        :clcm/node)
  (:import-from :cl-ppcre
                :scan)
  (:export :bullet-list-item-node
           :offset
           :attach-bullet-list-item!))
(in-package :clcm/nodes/bullet-list-item)

(defclass bullet-list-item-node (node)
  ((offset :accessor offset :initarg :offset)))

(defun attach-bullet-list-item! (node line offset)
  (multiple-value-bind (indent content) (get-indented-depth-and-line line offset)
    (let* ((list-item-offset (if (or (is-blank-line content) (>= indent 4)) 0 indent))
           (child (make-instance 'bullet-list-item-node :offset list-item-offset)))
      (add-child node child)
      (unless (is-blank-line content)
       (add!? child (subseq content list-item-offset) list-item-offset))
      child)))

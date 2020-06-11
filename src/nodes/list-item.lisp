(defpackage :clcm/nodes/list-item
  (:use :cl
        :clcm/utils
        :clcm/line
        :clcm/node)
  (:import-from :cl-ppcre
                :scan)
  (:export :list-item-node
           :offset
           :parent-is-tight
           :attach-list-item!))
(in-package :clcm/nodes/list-item)

(defclass list-item-node (node)
  ((offset :accessor offset :initarg :offset)
   (parent-is-tight :accessor parent-is-tight :initarg :parent-is-tight :initform t)))

(defun attach-list-item! (class node line offset)
  (multiple-value-bind (indent content) (get-indented-depth-and-line line offset)
    (let* ((list-item-offset (+ (if (or (is-blank-line content) (>= indent 4)) 0 indent) offset))
           (child (make-instance class :offset list-item-offset)))
      (add-child node child)
      (unless (is-blank-line content)
        (add!? child
               content
               offset))
      child)))

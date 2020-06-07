(defpackage :clcm/node
  (:use :cl)
  (:export :node
           :is-open
           :indent-level
           :children
           :close!?
           :add!?
           :parses-inlines
           :->html
           :close-node
           :last-child
           :add-child))
(in-package :clcm/node)

(defclass node ()
  ((is-open :accessor is-open :initarg :is-open :initform t)
   (children :accessor children :initarg :children :initform nil)))

(defgeneric close!? (node line offset))
(defgeneric add!? (node line offset))
(defgeneric parses-inlines (node))
(defgeneric ->html (node))

(defmethod parses-inlines ((node node))
  (setf (children node) (reverse (children node)))
  (loop :for child :in (children node)
        :if (typep child 'node)
        :do (parses-inlines child))
  node)

(defun close-node (node)
  (setf (is-open node) nil))

(defun last-child (node)
  (car (children node)))

(defun add-child (node new-child)
  (setf (children node) (cons new-child (children node))))


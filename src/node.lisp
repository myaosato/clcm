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
   (indent-level :accessor indent-level :initarg :indent-level :initform 0)
   (children :accessor children :initarg :children :initform nil)))

(defgeneric close!? (node line))
(defgeneric add!? (node line))
(defgeneric parses-inlines (node))
(defgeneric ->html (node))

(defmethod add!? :around ((node node) line)
  (let ((last-child (last-child node)))
    (if (and last-child
             (typep last-child 'node)
             (is-open last-child))
         (add!? last-child line)
         (call-next-method node line))))

(defmethod close!? :around ((node node) line)
  (call-next-method node line)
  (when (and (typep (last-child node) 'node)
             (is-open (last-child node)))
    (close!? (last-child node) line)))

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


(defpackage :clcm/tree
  (:use :cl :clcm/node)
  (:shadow :close)
  (:import-from :clcm/line
                :string->lines)
  (:import-from :clcm/utils
                :->)
  (:import-from :clcm/node
                :node
                :close!?
                :add!?
                :last-child)
  (:import-from :clcm/nodes/document
                :document-node)
  (:export :make-tree
           :tree->html))
(in-package :clcm/tree)

;; API
(defun make-tree (cm-string)
  (-> cm-string
      (cm->block-tree)
      (interprets-inlines)))

(defun tree->html (tree)
  (->html (root tree)))

;; tree
(defclass block-tree ()
  ((root :accessor root
         :initform (make-instance 'document-node))))

;; cm -> block-tree
(defun cm->block-tree (input-string)
  (let ((input-lines (string->lines input-string))
        (tree (make-instance 'block-tree)))
    (loop :for line :in input-lines
          ;; side effects !!
          :do (setf tree (consumes-line tree line)))
    tree))

(defun consumes-line (tree line)
  (close (root tree) line) ;; side effects !!
  (add (root tree) line) ;; side effects !!
  tree)

(defun close (node line)
  (when (typep node 'node)
    (close!? node line)
    (close (last-child node) line)))

(defun add (node line)
  (declare (type node node))
  (let ((last-child (last-child node)))
    (if (and last-child
             (typep last-child 'node)
             (is-open last-child))
        (add last-child line)
        (add!? node line))))

;; parse inlines
(defun interprets-inlines (tree)
  (parses-inlines (root tree))
  tree)

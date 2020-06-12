(defpackage :clcm/tree
  (:use :cl
        :clcm/utils
        :clcm/line
        :clcm/node
        :clcm/nodes/document)
  (:shadow :reverse-children)
  (:export :make-tree
           :tree->html))
(in-package :clcm/tree)

;; API
(defun make-tree (cm-string)
  (-> cm-string
      (cm->block-tree)
      (reverse-children)))

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
  (close!? (root tree) line 0) ;; side effects !!
  (add!? (root tree) line 0) ;; side effects !!
  tree)

;; parse inlines
(defun reverse-children (tree)
  (clcm/node:reverse-children (root tree))
  tree)

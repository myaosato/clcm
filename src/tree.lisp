(defpackage :clcm/tree
  (:use :cl
        :clcm/node
        :clcm/nodes/document)
  (:shadow :close)
  (:import-from :clcm/line
                :string->lines)
  (:import-from :clcm/utils
                :->)
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
  (close!? (root tree) line) ;; side effects !!
  (add!? (root tree) line 0) ;; side effects !!
  tree)

;; parse inlines
(defun interprets-inlines (tree)
  (parses-inlines (root tree))
  tree)

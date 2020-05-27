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
  (:import-from :clcm/nodes/block-quote
                :block-quote-node)
  (:import-from :clcm/nodes/block-quote-methods
                :trim-block-quote-marker)
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
    (cond ((typep node 'block-quote-node) ;; trim marker from line for children of block quote
           (close (last-child node) (trim-block-quote-marker line)))
          (t (close (last-child node) line)))))

(defun add (node line)
  (declare (type node node))
  (let ((last-child (last-child node)))
    (cond ((and last-child
                (typep last-child 'node)
                (is-open last-child))
           ;; trim marker from line for children of block quote
           (if (typep node 'block-quote-node)
               (add last-child (trim-block-quote-marker line))
               (add last-child line)))
          (t (add!? node line)))))

;; parse inlines
(defun interprets-inlines (tree)
  (parses-inlines (root tree))
  tree)

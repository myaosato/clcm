(defpackage :clcm/nodes/setext-heading
  (:use :cl :clcm/node)
  (:export :setext-heading-node
           :heading-level
           :is-setext-heading-level-1-line
           :is-setext-heading-level-2-line))
(in-package :clcm/nodes/setext-heading)

(defclass setext-heading-node (node)
  ((heading-level :accessor heading-level :initarg :heading-level :initform nil)))

(defmethod close!? ((node setext-heading-node) line)
  nil)

(defmethod add!? ((node setext-heading-node) line)
  nil)

(defmethod ->html ((node setext-heading-node))
  (let ((content (format nil "~{~A~^~%~}" (children node))))
    (format nil
            "<h~A>~A</h~A>~%"
            (heading-level node)
            content
            (heading-level node))))


;;
(defun is-setext-heading-level-1-line (line)
  (scan "^ {0,3}=+\\s*" line))

(defun is-setext-heading-level-2-line (line)
  (scan "^ {0,3}-+\\s*" line))

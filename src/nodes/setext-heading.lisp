(defpackage :clcm/nodes/setext-heading
  (:use :cl :clcm/node)
  (:export :setext-heading-node))
(in-package :clcm/nodes/setext-heading)

(defclass setext-heading-node (node)
  ((heading-level :accessor heading-level :initarg :heading-level :initform nil)))

(defmethod close!? ((node setext-heading-node) line)
  nil)

(defmethod add!? ((node setext-heading-node) line)
  nil)

(defmethod ->html ((node setext-heading-node))
  (let ((content (format nil "~{~A~^~%~}" (reverse (children node)))))
    (format nil
            "<h~A>~A</h~A>~%"
            (heading-level node)
            content
            (heading-level node))))

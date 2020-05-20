(defpackage :clcm/nodes/paragraph
  (:use :cl :clcm/node)
  (:import-from :clcm/line
                :is-blank-line
                :is-atx-heading-line
                :is-setext-heading-level-1-line
                :is-setext-heading-level-2-line
                :*white-space-characters*)
  (:import-from :clcm/nodes/setext-heading
                :setext-heading-node
                :heading-level)
  (:export :paragraph-node))
(in-package :clcm/nodes/paragraph)

(defclass paragraph-node (node)
  ())

(defmethod close!? ((node paragraph-node) line)
  (when (or (is-blank-line line)
            (is-atx-heading-line line))
    (close-node node)))

(defmethod add!? ((node paragraph-node) line)
  (cond ((is-setext-heading-level-1-line line)
         (change-class node 'setext-heading-node)
         (setf (heading-level node) 1)
         (close-node node))
        ((is-setext-heading-level-2-line line)
         (change-class node 'setext-heading-node)
         (setf (heading-level node) 2)
         (close-node node))
        (t
         (add-child node (string-trim *white-space-characters* line)))))

(defmethod ->html ((node paragraph-node))
  (format nil "<p>~{~A~^~%~}</p>~%" (reverse (children node))))

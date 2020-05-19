(defpackage :clcm/nodes/paragraph
  (:use :cl :clcm/node)
  (:import-from :clcm/line
                :is-blank-line
                :is-atx-heading-line
                :*white-space-characters*)
  (:export :paragraph-node))
(in-package :clcm/nodes/paragraph)

(defclass paragraph-node (node)
  ())

(defmethod close!? ((node paragraph-node) line)
  (when (or (is-blank-line line)
            (is-atx-heading-line line))
    (close-node node)))

(defmethod add!? ((node paragraph-node) line)
  (add-child node (string-trim *white-space-characters* line)))

(defmethod ->html ((node paragraph-node))
  (format nil "<p>~{~A~^~%~}</p>~%" (reverse (children node))))

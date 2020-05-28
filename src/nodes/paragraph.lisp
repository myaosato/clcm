(defpackage :clcm/nodes/paragraph
  (:use :cl :clcm/node)
  (:import-from :clcm/line
                :is-blank-line
                :is-thematic-break-line
                :is-atx-heading-line
                :is-setext-heading-level-1-line
                :is-setext-heading-level-2-line
                :is-backtick-fenced-code-block-line
                :is-tilde-fenced-code-block-line
                :is-html-block-type-1-start-line
                :is-html-block-type-2-start-line
                :is-html-block-type-3-start-line
                :is-html-block-type-4-start-line
                :is-html-block-type-5-start-line
                :is-html-block-type-6-start-line
                :is-block-quote-line
                :*white-space-characters*)
  (:import-from :clcm/nodes/setext-heading
                :setext-heading-node
                :heading-level)
  (:export :paragraph-node))
(in-package :clcm/nodes/paragraph)

(defclass paragraph-node (node)
  ())

(defun close-paragraph-line (line)
  (or (is-blank-line line)
      (and (not (is-setext-heading-level-2-line line))
           (is-thematic-break-line line))
      (is-atx-heading-line line)
      (is-backtick-fenced-code-block-line line)
      (is-tilde-fenced-code-block-line line)
      (is-html-block-type-1-start-line line)
      (is-html-block-type-2-start-line line)
      (is-html-block-type-3-start-line line)
      (is-html-block-type-4-start-line line)
      (is-html-block-type-5-start-line line)
      (is-html-block-type-6-start-line line)
      (is-block-quote-line line)))

(defmethod close!? ((node paragraph-node) line)
  (when (close-paragraph-line line)
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
  (let ((content (format nil "~{~A~^~%~}" (children node))))
    (format nil "<p>~A</p>~%" content)))

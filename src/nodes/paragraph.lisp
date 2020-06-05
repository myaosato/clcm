(defpackage :clcm/nodes/paragraph
  (:use :cl
        :clcm/line
        :clcm/node
        :clcm/nodes/thematic-break
        :clcm/nodes/atx-heading
        :clcm/nodes/setext-heading
        :clcm/nodes/fenced-code-block
        :clcm/nodes/html-block
        :clcm/nodes/block-quote)
  (:export :paragraph-node
           :attach-paragraph!))
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
      (is-html-block-line '(1 2 3 4 5 6) line)
      (is-block-quote-line line)))

(defmethod close!? ((node paragraph-node) line)
  (when (close-paragraph-line line)
    (close-node node)))

(defmethod add!? ((node paragraph-node) line offset)
  (declare (ignore offset))
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

(defun attach-paragraph! (node line)
  (let ((child (make-instance 'paragraph-node)))
    (add-child node child)
    (add!? child line 0)
    child))

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
  ((can-change-heading :accessor can-change-heading :initarg :can-change-heading :initform t)))

;; close
(defun close-paragraph-line (line offset)
  (or (is-blank-line line)
      (and (not (is-setext-heading-level-2-line line))
           (is-thematic-break-line line offset))
      (is-atx-heading-line line offset)
      (is-backtick-fenced-code-block-line line offset)
      (is-tilde-fenced-code-block-line line offset)
      (is-html-block-line '(1 2 3 4 5 6) line)
      (is-block-quote-line line offset)))

(defmethod close!? ((node paragraph-node) line offset)
  (when (close-paragraph-line line offset)
    (close-node node)))

;; add
(defmethod add!? ((node paragraph-node) line offset)
  (declare (ignore offset))
  (cond ((and (is-setext-heading-level-1-line line) (children node) (can-change-heading node))
         (change-class node 'setext-heading-node)
         (setf (heading-level node) 1)
         (close-node node))
        ((and (is-setext-heading-level-2-line line) (children node) (can-change-heading node))
         (change-class node 'setext-heading-node)
         (setf (heading-level node) 2)
         (close-node node))
        (t
         (add-child node (string-trim *white-space-characters* line)))))

;; ->html
(defmethod ->html ((node paragraph-node))
  (let ((content (format nil "~{~A~^~%~}" (children node))))
    (format nil "<p>~A</p>~%" content)))

(defun attach-paragraph! (node line &key (can-change-heading t))
  (let ((child (make-instance 'paragraph-node :can-change-heading can-change-heading)))
    (add-child node child)
    (add!? child line 0)
    child))

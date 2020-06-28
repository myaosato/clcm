(defpackage :clcm/nodes/paragraph
  (:use :cl
        :clcm/line
        :clcm/inlines/inlines
        :clcm/node
        :clcm/nodes/thematic-break
        :clcm/nodes/atx-heading
        :clcm/nodes/setext-heading
        :clcm/nodes/fenced-code-block
        :clcm/nodes/html-block
        :clcm/nodes/block-quote
        :clcm/nodes/bullet-list
        :clcm/nodes/ordered-list)
  (:export :paragraph-node
           :not-render-tag
           :attach-paragraph!))
(in-package :clcm/nodes/paragraph)

(defclass paragraph-node (node)
  ((can-change-heading :accessor can-change-heading :initarg :can-change-heading :initform t)
   (not-render-tag :accessor not-render-tag :initarg :not-render-tag :initform nil)))

;; close
(defun close-paragraph-line (line offset)
  (or (is-blank-line line)
      (and (not (is-setext-heading-level-2-line line))
           (is-thematic-break-line line offset))
      (is-atx-heading-line line offset)
      (is-backtick-fenced-code-block-line line offset)
      (is-tilde-fenced-code-block-line line offset)
      (is-html-block-line '(1 2 3 4 5 6) line offset)
      (is-block-quote-line line offset)
      (is-bullet-list-line line offset)
      (is-ordered-list-line line offset)))

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
         (add-child node (string-left-trim *white-space-characters* line)))))

;; ->html
(defmethod ->html ((node paragraph-node))
  (let ((content (inlines->html (children node))))
    (if (not-render-tag node)
        (format nil "~A" content)
        (format nil "<p>~A</p>~%" content))))

(defun attach-paragraph! (node line &key (can-change-heading t))
  (let ((child (make-instance 'paragraph-node :can-change-heading can-change-heading)))
    (add-child node child)
    (add!? child line 0)
    child))

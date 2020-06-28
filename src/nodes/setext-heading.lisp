(defpackage :clcm/nodes/setext-heading
  (:use :cl
        :clcm/line
        :clcm/inlines/inlines
        :clcm/node)
  (:import-from :cl-ppcre
                :scan)
  (:export :setext-heading-node
           :heading-level
           :is-setext-heading-level-1-line
           :is-setext-heading-level-2-line))
(in-package :clcm/nodes/setext-heading)

(defclass setext-heading-node (node)
  ((heading-level :accessor heading-level :initarg :heading-level :initform nil)))

;; close  not use
;;(defmethod close!? ((node setext-heading-node) line offset) nil)

;; add  not use
;;(defmethod add!? ((node setext-heading-node) line offset) nil)

;; ->html
(defmethod ->html ((node setext-heading-node))
  (let ((content (inlines->html (mapcar #'(lambda (line) 
                                            (string-trim *white-space-characters* line))
                                        (children node)))))
    (format nil
            "<h~A>~A</h~A>~%"
            (heading-level node)
            content
            (heading-level node))))

;;
(defun is-setext-heading-level-1-line (line)
  (scan "^ {0,3}=+\\s*$" line))

(defun is-setext-heading-level-2-line (line)
  (scan "^ {0,3}-+\\s*$" line))

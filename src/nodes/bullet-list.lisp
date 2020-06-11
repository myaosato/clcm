(defpackage :clcm/nodes/bullet-list
  (:use :cl
        :clcm/line
        :clcm/node
        :clcm/nodes/list)
  (:import-from :cl-ppcre
                :scan)
  (:export :bullet-list-node
           :marker
           :is-tight
           :is-bullet-list-line
           :attach-bullet-list!?))
(in-package :clcm/nodes/bullet-list)

(defclass bullet-list-node (list-node)
  ((marker :accessor marker :initarg :marker)))

;; block quote
(defun is-bullet-list-line (line offset)
  (multiple-value-bind (indent content) (get-indented-depth-and-line line offset)
    (and (<= indent 3) (scan "^(-|\\+|\\*)(?:\\s|\\t|$)" content :start indent))))

(defun attach-bullet-list!? (node line offset)
  (let ((marker-pos (is-bullet-list-line line offset)))
    (when marker-pos
      (let ((child (make-instance 'bullet-list-node :marker (char line marker-pos))))
        (add-child node child)
        (add!? child line offset)
        child))))

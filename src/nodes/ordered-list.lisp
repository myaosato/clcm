(defpackage :clcm/nodes/ordered-list
  (:use :cl
        :clcm/line
        :clcm/node
        :clcm/nodes/list)
  (:import-from :cl-ppcre
                :scan-to-strings)
  (:export :ordered-list-node
           :is-tight
           :start
           :is-ordered-list-line
           :attach-ordered-list!?))
(in-package :clcm/nodes/ordered-list)

(defclass ordered-list-node (list-node)
  ((start :accessor start :initarg :start)))

;; block quote
(defun is-ordered-list-line (line offset)
  (multiple-value-bind (indent content) (get-indented-depth-and-line line offset)
    (if (> indent 3) (return-from is-ordered-list-line nil))
    (multiple-value-bind (result marker)
        (scan-to-strings "^(\\d{1,9}(:?\\.|\\)))(?:\\s|\\t|$)" content :start indent)
      (if result
          (aref marker 0)))))

(defun attach-ordered-list!? (node line offset)
  (let ((marker (is-ordered-list-line line offset)))
    (when marker
      (let ((child (make-instance 'ordered-list-node
                                  :start (parse-integer (subseq marker 0 (1- (length marker)))))))
        (add-child node child)
        (add!? child line offset)
        child))))

(defpackage :clcm/nodes/thematic-break
  (:use :cl
        :clcm/line
        :clcm/node)
  (:import-from :cl-ppcre
                :scan)
  (:export :thematic-break-node
           :is-thematic-break-line
           :attach-thematic-break!?))
(in-package :clcm/nodes/thematic-break)

;;
(defclass thematic-break-node (node)
  ())

;; close  not use
;;(defmethod close!? ((node thematic-break-node) line offset)  nil)

;; add  not use
;;(defmethod add!? ((node thematic-break-node) line offset) nil)

;; ->html
(defmethod ->html ((node thematic-break-node))
  (format nil "<hr />~%"))

;;
(defun is-thematic-break-line (line offset)
  (multiple-value-bind (indent contents) (get-indented-depth-and-line line offset)
    (and (<= indent 3) 
         (or (scan "^(?:\\*\\s*){3,}$" line :start indent)
             (scan "^(?:_\\s*){3,}$" line :start indent)
             (scan "^(?:-\\s*){3,}$" line :start indent)))))

(defun attach-thematic-break!? (node line offset)
  (when (is-thematic-break-line line offset)
    (let ((child (make-instance 'thematic-break-node :is-open nil)))
      (add-child node child)
      child)))

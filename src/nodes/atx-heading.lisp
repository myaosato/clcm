(defpackage :clcm/nodes/atx-heading
  (:use :cl
        :clcm/line
        :clcm/utils
        :clcm/node)
  (:import-from :cl-ppcre
                :scan
                :scan-to-strings)
  (:export :atx-heading-node
           :is-atx-heading-line
           :attach-atx-heading!?))
(in-package :clcm/nodes/atx-heading)

(defclass atx-heading-node (node)
  ((heading-level :accessor heading-level :initarg :heading-level :initform nil)))

;; close
;;(defmethod close!? ((node atx-heading-node) line offset) nil)

;; add
(defun get-content (line)
  (->> line ; "  ###  ho  ge  ## "
       (string-left-trim '(#\Space #\Tab)) ; "###  ho  ge  ## "
       (string-right-trim '(#\Space)) ; "###  ho  ge  ##"
       (string-left-trim '(#\#)) ; "  ho  ge  ##"
       (trim-closing-sequence) ; "  ho  ge  "
       (string-trim '(#\Space #\Tab)))); "ho  ge"

(defun get-level (line)
  (->> line
       (string-trim '(#\Space))
       (scan-to-strings "^#{1,6}")
       (length)))

(defmethod add!? ((node atx-heading-node) line offset)
  (declare (ignore offset))
  (let ((content (get-content line))
        (level (get-level line)))
    (setf (heading-level node) level)
    (setf (children node) (list content))))

;; html
(defun trim-closing-sequence (line)
  (let ((candidate (string-right-trim '(#\#) line)))
    (if (or (string= candidate "")
            (char= (last-char candidate) #\Space)
            (char= (last-char candidate) #\Tab))
        candidate
        line)))

(defmethod ->html ((node atx-heading-node))
  (let ((content (first (children node))))
    (format nil
            "<h~A>~A</h~A>~%"
            (heading-level node)
            content
            (heading-level node))))

;;
(defun is-atx-heading-line (line offset)
  (multiple-value-bind (indent content) (get-indented-depth-and-line line offset)
    (and (<= indent 3) (scan "^#{1,6}(\\t| |$)" content :start indent))))

(defun attach-atx-heading!? (node line offset)
  (when (is-atx-heading-line line offset)
    (let ((child (make-instance 'atx-heading-node :is-open nil)))
      (add-child node child)
      (add!? child line offset)
      child)))

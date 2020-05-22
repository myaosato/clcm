(defpackage :clcm/nodes/atx-heading
  (:use :cl :clcm/node)
  (:import-from :cl-ppcre
                :scan-to-strings)
  (:import-from :clcm/utils
                :->>
                :last-char)
  (:export :atx-heading-node))
(in-package :clcm/nodes/atx-heading)

(defclass atx-heading-node (node)
  ((heading-level :accessor heading-level :initarg :heading-level :initform nil)))

(defmethod close!? ((node atx-heading-node) line)
  nil)

(defmethod add!? ((node atx-heading-node) line)
  (let ((content (get-content line))
        (level (get-level line)))
    (setf (heading-level node) level)
    (setf (children node) (list content))))

(defun trim-closing-sequence (line)
  (let ((candidate (string-right-trim '(#\#) line)))
    (if (or (string= candidate "")
            (char= (last-char candidate) #\Space)
            (char= (last-char candidate) #\Tab))
        candidate
        line)))

(defun get-content (line)
  (->> line ; "  ###  ho  ge  ## "
       (string-trim '(#\Space)) ; "###  ho  ge  ##"
       (string-left-trim '(#\#)) ; "  ho  ge  ##"
       (trim-closing-sequence) ; "  ho  ge  "
       (string-trim '(#\Space #\Tab)))); "ho  ge"

(defun get-level (line)
  (->> line
       (string-trim '(#\Space))
       (scan-to-strings "^#{1,6}")
       (length)))

(defmethod ->html ((node atx-heading-node))
  (let ((content (first (children node))))
    (format nil
            "<h~A>~A</h~A>~%"
            (heading-level node)
            content
            (heading-level node))))

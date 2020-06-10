(defpackage :clcm/nodes/bullet-list-methods
  (:use :cl
        :clcm/utils
        :clcm/line
        :clcm/node
        :clcm/nodes/bullet-list
        :clcm/nodes/bullet-list-item)
  (:import-from :cl-ppcre
                :scan)
  (:export))
(in-package :clcm/nodes/bullet-list-methods)

(defun trim-marker (line offset)
  (multiple-value-bind (indent content) (get-indented-depth-and-line line offset)
    (cond ((= (length content) (1+ indent))
           (list (+ indent 2) ""))
          ((char= (char content (1+ indent)) #\Space)
           (list (+ indent 2) (subseq content (+ indent 2))))
          ((char= (char content (1+ indent)) #\Tab)
           (list (+ indent 2) 
                 (format nil "~A~A"
                             (repeat-char #\Space (- 3 (mod (1+ indent) 4)))
                             (subseq content (+ indent 2))))))))

;; close
(defmethod close!? ((node bullet-list-node) line offset)
  (close!? (last-child node) line offset)
  (let ((marker-pos (is-bullet-list-line line offset)))
    (unless (or (and marker-pos (char= (char line marker-pos) (marker node)))
                (and (is-open (last-child node))))
      (close-node node))))

;; add
(defun _add!? (node line offset)
  (destructuring-bind (marker-offset content) (trim-marker line offset)
    (attach-bullet-list-item! node content marker-offset)))

(defmethod add!? ((node bullet-list-node) line offset)
  (let ((last-child (last-child node)))
    (if (not (and last-child (is-open last-child)))
        (_add!? node line offset)
        (add!? last-child line offset))))

;; ->html
(defmethod ->html ((node bullet-list-node))
  (format nil "<ul>~%~{~A~}</ul>~%"
          (mapcar #'->html (children node))))

(defpackage :clcm/nodes/bullet-list-methods
  (:use :cl
        :clcm/utils
        :clcm/line
        :clcm/node
        :clcm/nodes/blank-line
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
    (attach-bullet-list-item! node content (+ offset marker-offset))))

(defmethod add!? ((node bullet-list-node) line offset)
  (let ((last-child (last-child node)))
    (cond ((and (is-tight node) (is-blank-line line))
           (setf (is-tight node) 'foo))
          ((and (eq (is-tight node) 'foo) (not (and last-child (is-open last-child))))
           (setf (is-tight node) nil))
          ((eq (is-tight node) 'foo)
           (setf (is-tight node) t)))
    (if (and last-child (is-open last-child))
        (add!? last-child line offset)
        (_add!? node line offset))))


;; ->html
(defmethod ->html ((node bullet-list-node))
  (let ((items (children node)))
    (if (find-if (lambda (item)
                   (let ((children (children item)))
                     (and (>= (length children) 3)
                          (find-if (lambda (elt) (typep elt 'blank-line-node))
                                   children
                                   :start 1 :end (1- (length children))))))
                 items)
        (setf (is-tight node) nil))
    (unless (is-tight node)
      (loop :for child :in items
            :do (setf (parent-is-tight child) nil)))
    (format nil "<ul>~%~{~A~}</ul>~%"
            (mapcar #'->html items))))

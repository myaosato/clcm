(defpackage :clcm/nodes/ordered-list-methods
  (:use :cl
        :clcm/utils
        :clcm/line
        :clcm/node
        :clcm/nodes/blank-line
        :clcm/nodes/ordered-list
        :clcm/nodes/ordered-list-item)
  (:import-from :cl-ppcre
                :scan)
  (:export))
(in-package :clcm/nodes/ordered-list-methods)

(defun trim-marker (line offset)
  (let ((marker-width (length (is-ordered-list-line line offset))))
    (multiple-value-bind (indent content) (get-indented-depth-and-line line offset)
      (let ((marker-end (+ indent marker-width)))
        (cond ((= (length content) marker-end)
               (list (1+ marker-end) ""))
              ((char= (char content marker-end) #\Space)
               (list (1+ marker-end) (subseq content (1+ marker-end))))
              ((char= (char content marker-end) #\Tab)
               (list (1+ marker-end)
                     (format nil "~A~A"
                             (repeat-char #\Space (- 3 (mod (1+ indent) 4)))
                             (subseq content (1+ marker-end))))))))))

;; close
(defmethod close!? ((node ordered-list-node) line offset)
  (close!? (last-child node) line offset)
  (unless (or (is-ordered-list-line line offset)
              (is-open (last-child node)))
    (close-node node)))

;; add
(defun _add!? (node line offset)
  (destructuring-bind (marker-offset content) (trim-marker line offset)
    (attach-ordered-list-item! node content marker-offset)))

(defmethod add!? ((node ordered-list-node) line offset)
  (let ((last-child (last-child node)))
    (cond ((and (is-tight node) (is-blank-line line))
           (setf (is-tight node) 'foo))
          ((and (eq (is-tight node) 'foo) (not (and last-child (is-open last-child))))
           (setf (is-tight node) nil))
          ((eq (is-tight node) 'foo)
           (setf (is-tight node) t)))
    (if (not (and last-child (is-open last-child)))
        (_add!? node line offset)
        (add!? last-child line offset))))

;; ->html
(defmethod ->html ((node ordered-list-node))
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
    (format nil "<ol~A>~%~{~A~}</ol>~%"
            (if (= 1 (start node)) "" (format nil " start=\"~A\"" (start node)))
            (mapcar #'->html items))))

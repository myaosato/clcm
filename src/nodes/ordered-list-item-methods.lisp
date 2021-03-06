(defpackage :clcm/nodes/ordered-list-item-methods
  (:use :cl
        :clcm/line
        :clcm/node
        :clcm/container-utils
        :clcm/nodes/thematic-break
        :clcm/nodes/atx-heading
        :clcm/nodes/indented-code-block
        :clcm/nodes/fenced-code-block
        :clcm/nodes/html-block
        :clcm/nodes/paragraph
        :clcm/nodes/blank-line
        :clcm/nodes/block-quote
        :clcm/nodes/bullet-list
        :clcm/nodes/ordered-list
        :clcm/nodes/ordered-list-item)
  (:import-from :cl-ppcre
                :scan)
  (:export))
(in-package :clcm/nodes/ordered-list-item-methods)

;; for paragraph in block quote
(defun close-paragraph-line (line offset)
  (or (is-blank-line line)
      (is-thematic-break-line line offset)
      (is-atx-heading-line line offset)
      (is-backtick-fenced-code-block-line line offset)
      (is-tilde-fenced-code-block-line line offset)
      (is-html-block-line '(1 2 3 4 5 6) line offset)
      (is-block-quote-line line offset)
      (is-bullet-list-line line offset)
      (is-ordered-list-line line offset)))

;; close
(defmethod close!? ((node ordered-list-item-node) line offset)
  (multiple-value-bind (indent content) (get-indented-depth-and-line line offset :limit (offset node))
    (cond ((is-blank-line line)
           (if (and (last-child node) (is-open (last-child node)))
               (close!? (last-child node) line offset)))
          ((< indent (- (offset node) offset))
           (close-node node))
          ((and (last-child node) (is-open (last-child node)))
           (close!? (last-child node) (subseq content (- (offset node) offset)) (offset node))))))

;; add
(defmethod add!? ((node ordered-list-item-node) line offset)
  (multiple-value-bind (indent content) (get-indented-depth-and-line line offset :limit (offset node))
    (declare (ignore indent))
    (let ((trimed-line (if (is-blank-line line)
                           content
                           (subseq content (- (offset node) offset))))
          (child-offset (if (is-blank-line line)
                            0
                            (offset node)))
          (last-child (last-child node)))
      (cond ((and last-child (is-open last-child))
             (add!? last-child trimed-line child-offset))
            (t
             (or (when (is-blank-line trimed-line)
                   (push (make-instance 'blank-line-node :is-open nil) (children node)))
                 (attach-thematic-break!? node trimed-line child-offset)
                 (attach-atx-heading!? node trimed-line child-offset)
                 (attach-indented-code-block!? node trimed-line child-offset)
                 (attach-fenced-code-block!? node trimed-line child-offset)
                 (attach-html-block!? node trimed-line child-offset)
                 (attach-block-quote!? node trimed-line child-offset)
                 (attach-bullet-list!? node trimed-line child-offset)
                 (attach-ordered-list!? node trimed-line child-offset)
                 (attach-paragraph! node trimed-line)))))))

;; ->html
(defmethod ->html ((node ordered-list-item-node))
    (let ((children (remove-if (lambda (child)
                               (typep child 'blank-line-node))
                             (children node))))
    (cond ((parent-is-tight node)
           (loop :for child :in children
                 :if (typep child 'paragraph-node)
                 :do (setf (not-render-tag child) t))
           (if (or (= (length children) 0) (typep (first children) 'paragraph-node))
               (format nil "<li>~{~A~^~%~}</li>~%" (mapcar #'->html children))
               (format nil "<li>~%~{~A~^~%~}</li>~%" (mapcar #'->html children))))
          (t (format nil "<li>~%~{~A~}</li>~%" (mapcar #'->html children))))))

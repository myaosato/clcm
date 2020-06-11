(defpackage :clcm/nodes/bullet-list-item-methods
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
        :clcm/nodes/block-quote
        :clcm/nodes/bullet-list
        :clcm/nodes/bullet-list-item)
  (:import-from :cl-ppcre
                :scan)
  (:export))
(in-package :clcm/nodes/bullet-list-item-methods)

;; for paragraph in block quote
(defun close-paragraph-line (line offset)
  (or (is-blank-line line)
      (is-thematic-break-line line offset)
      (is-atx-heading-line line offset)
      (is-backtick-fenced-code-block-line line offset)
      (is-tilde-fenced-code-block-line line offset)
      (is-html-block-line '(1 2 3 4 5 6) line offset)
      (is-block-quote-line line offset)
      (is-bullet-list-line line offset)))

;; close
(defmethod close!? ((node bullet-list-item-node) line offset)
  (multiple-value-bind (indent content) (get-indented-depth-and-line line offset :limit (offset node))
    (cond ((is-blank-line line)
           (if (last-child node)
               (close!? (last-child node) line offset)))
          ((has-paragraph-as-last node)
           (if (close-paragraph-line line offset)
               (close-node node)))
          ((< indent (offset node))
           (close-node node))
          ((last-child node)
           (close!? (last-child node) (subseq content (- (offset node) offset)) offset)))))

;; add
(defmethod add!? ((node bullet-list-item-node) line offset)
  (multiple-value-bind (indent content) (get-indented-depth-and-line line offset :limit (offset node))
    (declare (ignore indent))
    (let ((trimed-line (if (is-blank-line line)
                           content
                           (subseq content (- (offset node) offset))))
          (child-offset (if (is-blank-line line)
                            0
                            (offset node))))
      (or (skip-blank-line? trimed-line)
          (attach-thematic-break!? node trimed-line child-offset)
          (attach-atx-heading!? node trimed-line child-offset)
          (attach-indented-code-block!? node trimed-line child-offset)
          (attach-fenced-code-block!? node trimed-line child-offset)
          (attach-html-block!? node trimed-line child-offset)
          (attach-block-quote!? node trimed-line child-offset)
          (attach-bullet-list!? node trimed-line child-offset)
          (attach-paragraph! node trimed-line)))))

;; ->html
(defmethod ->html ((node bullet-list-item-node))
  (if (and (parent-is-tight node)
           (typep (first (children node)) 'paragraph-node))
      (format nil "<li>~{~A~^~%~}</li>~%" (children (first (children node))))
      (format nil "<li>~%~{~A~}</li>~%" (mapcar #'->html (children node)))))

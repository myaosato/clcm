(defpackage :clcm/nodes/block-quote-methods
  (:use :cl
        :clcm/utils
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
        :clcm/nodes/bullet-list)
  (:import-from :cl-ppcre
                :scan-to-strings)
  (:export :trim-block-quote-marker))
(in-package :clcm/nodes/block-quote-methods)

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

(defun trim-block-quote-marker (line offset)
  ;; return (TRIMED-CONTENTS OFFSET)
  (multiple-value-bind (indent _line) (get-indented-depth-and-line line offset)
    (if (>= indent 4) (error "line is not block-quote-line: ~A" line))
    (multiple-value-bind (has-marker contents) (scan-to-strings "^>(.*)" _line :start indent)
      (if (not has-marker) (error "line is not block-quote-line: ~A" line))
      (let ((content (aref contents 0)))
        ;; return (TRIMED-CONTENTS OFFSET)
        (cond ((string= content "") (list content (1+ indent)))
              ((char= (char content 0) #\Space) (list (subseq content 1) (+ indent 2)))
              ((char= (char content 0) #\Tab)
               (list (format nil "~A~A"
                             (repeat-char #\Space (- 3 (mod (1+ indent) 4)))
                             (subseq content 1))
                     (+ indent 2)))
              (t (list content (1+ indent))))))))

;; close
(defun _close!? (node line offset)
  (let* ((last-child (last-child node))
         (has-paragraph-as-last (has-paragraph-as-last node))
         (is-block-quote-line (is-block-quote-line line offset)))
    (cond ((and (not has-paragraph-as-last) (not is-block-quote-line))
           (if (and last-child (is-open last-child))
               (close-node last-child))
           (close-node node))
          (has-paragraph-as-last
           ;; tihs dispatch (destructuring-bind (...) (if is-block-quote-line ...)
           ;; for lazy continuation line
           (destructuring-bind (trimed-line child-offset) (if is-block-quote-line
                                                              (trim-block-quote-marker line offset)
                                                              (list line offset))
             (when (close-paragraph-line trimed-line child-offset)
               (close-node last-child)
               (unless (is-block-quote-line line offset)
                 (close-node node))))))))

(defmethod close!? ((node block-quote-node) line offset)
  (_close!? node line offset) ;; <- last child paragraph checked here (*)
  (let ((last-child (last-child node)))
    (when (and last-child (is-open last-child) (not (has-paragraph-as-last node)))
      (destructuring-bind (trimed-line child-offset) (trim-block-quote-marker line offset)
        (close!? last-child trimed-line child-offset)))))

;; add
(defun _add!? (node line offset)
  (destructuring-bind (trimed-line child-offset) (trim-block-quote-marker line offset)
    (or (skip-blank-line? trimed-line)
        (attach-thematic-break!? node trimed-line child-offset)
        (attach-atx-heading!? node trimed-line child-offset)
        (attach-indented-code-block!? node trimed-line child-offset)
        (attach-fenced-code-block!? node trimed-line child-offset)
        (attach-html-block!? node trimed-line child-offset)
        (attach-block-quote!? node trimed-line child-offset)
        (attach-bullet-list!? node trimed-line child-offset)
        (attach-paragraph! node trimed-line :can-change-heading nil))))

(defmethod add!? ((node block-quote-node) line offset)
  (let ((last-child (last-child node)))
    (when (not (and last-child (is-open last-child)))
      (return-from add!? (_add!? node line offset)))
    (if (and (has-paragraph-as-last node) (not (is-block-quote-line line offset)))
        ;; lazy continuation line
        (add!? last-child line offset)
        (destructuring-bind (trimed-line child-offset) (trim-block-quote-marker line offset)
          (add!? last-child trimed-line child-offset)))))

;; ->html
(defmethod ->html ((node block-quote-node))
  (format nil "<blockquote>~%~{~A~}</blockquote>~%"
          (mapcar #'->html (children node))))

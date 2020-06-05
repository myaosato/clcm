(defpackage :clcm/nodes/fenced-code-block
  (:use :cl :clcm/node)
  (:import-from :cl-ppcre
                :scan
                :scan-to-strings)
  (:import-from :clcm/utils
                :repeat-char
                :trim-left-space-max-n)
  (:export :fenced-code-block-node
           :is-backtick-fenced-code-block-line
           :is-tilde-fenced-code-block-line
           :attach-fenced-code-block!?))
(in-package :clcm/nodes/fenced-code-block)

(defclass fenced-code-block-node (node)
  ((code-fence-indent :accessor code-fence-indent :initarg :code-fence-indent)
   (code-fence-character :accessor code-fence-character :initarg :code-fence-character)
   (code-fence-length :accessor code-fence-length :initarg :code-fence-length)
   (info-string :accessor info-string :initarg :info-string)))

(defmethod close!? ((node fenced-code-block-node) line)
  ;; close when call add!?
  nil)

(defmethod add!? ((node fenced-code-block-node) line offset)
  (declare (ignore offset))
  (cond ((is-closed-line node line)
         (close-node node))
        ((= (code-fence-indent node) 0)
         (add-child node line))
        ((string= line "")
         (add-child node line))
        ((char= (char line 0) #\Tab)
         (add-child node
                    (concatenate 'string
                                 (repeat-char #\Space (- 4 (code-fence-indent node)))
                                 (subseq line 1))))
        (t (add-child node (trim-left-space-max-n line (code-fence-indent node))))))

(defmethod ->html ((node fenced-code-block-node))
  (let ((content (format nil "~{~A~%~}" (children node))))
    (format nil
            "<pre><code~A>~A</code></pre>~%"
            (if (string/= (info-string node) "")
                (format nil " class=\"language-~A\"" (info-string node))
                "")
            content)))

(defun get-code-fence (line)
  (or (second (multiple-value-list (is-backtick-fenced-code-block-line line)))
      (second (multiple-value-list (is-tilde-fenced-code-block-line line)))))

(defun is-closed-line (node line) ;; node :fenced-code-block-node
  (let ((close-regexp (format nil "^ {0,3}~A{~A,} *$"
                              (code-fence-character node) (code-fence-length node))))
    (scan close-regexp line)))


;;
(defun make-fenced-code-block-node (line)
  (let ((code-fence-info (get-code-fence line)))
    (if code-fence-info
        (make-instance 'fenced-code-block-node
                       :code-fence-indent (length (aref code-fence-info 0))
                       :code-fence-character (char (aref code-fence-info 1) 0)
                       :code-fence-length (length (aref code-fence-info 1))
                       :info-string (aref code-fence-info 2)))))

(defun is-backtick-fenced-code-block-line (line)
  (scan-to-strings "^( {0,3})(`{3,})\\s*([^`\\s]*)(?:\\s[^`]*)?$" line))

(defun is-tilde-fenced-code-block-line (line)
  (scan-to-strings "^( {0,3})(~{3,})\\s*([^\\s]*)(?:\\s.*)?$" line))

(defun attach-fenced-code-block!? (node line)
  (when (or (is-backtick-fenced-code-block-line line)
            (is-tilde-fenced-code-block-line line))
    (let ((child (make-fenced-code-block-node line)))
      (add-child node child)
      child)))

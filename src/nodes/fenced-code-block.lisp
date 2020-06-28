(defpackage :clcm/nodes/fenced-code-block
  (:use :cl
        :clcm/utils
        :clcm/line
        :clcm/inlines/inlines
        :clcm/inlines/inlines-ltgtamp
        :clcm/node)
  (:import-from :cl-ppcre
                :scan
                :scan-to-strings)
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

;; close
;; do not anything
;; because if a line closes a node by close!?, this line open another fenced-code-block-node
(defmethod close!? ((node fenced-code-block-node) line offset) nil)

(defun is-closed-line (node line offset) ;; node :fenced-code-block-node
  (multiple-value-bind (indent content) (get-indented-depth-and-line line offset)
    (and (<= indent 3)
         (let ((close-regexp (format nil "^~A{~A,} *$"
                                     (code-fence-character node) (code-fence-length node))))
           (scan close-regexp content :start indent)))))

;; add
(defmethod add!? ((node fenced-code-block-node) line offset)
  (when (is-closed-line node line offset)
    (close-node node)
    (return-from add!?))
  (multiple-value-bind (indent content) (get-indented-depth-and-line line offset)
    (if (< indent (code-fence-indent node))
        (add-child node (subseq content indent))
        (add-child node (subseq content (code-fence-indent node))))))

;; ->html
(defmethod ->html ((node fenced-code-block-node))
  (let ((content (<>&->ref (children node) :last-break t)))
    (format nil
            "<pre><code~A>~A</code></pre>~%"
            (if (string/= (info-string node) "")
                (format nil " class=\"language-~A\"" (inlines->html (list (info-string node))))
                "")
            content)))

;;
(defun get-code-fence (line offset)
  (or (is-backtick-fenced-code-block-line line offset)
      (is-tilde-fenced-code-block-line line offset)))


(defun make-fenced-code-block-node (line offset)
  (let ((code-fence-info (get-code-fence line offset)))
    (if code-fence-info
        (make-instance 'fenced-code-block-node
                       :code-fence-indent (length (aref code-fence-info 0))
                       :code-fence-character (char (aref code-fence-info 1) 0)
                       :code-fence-length (length (aref code-fence-info 1))
                       :info-string (aref code-fence-info 2)))))

(defun is-backtick-fenced-code-block-line (line offset)
  (multiple-value-bind (indent content) (get-indented-depth-and-line line offset)
    (if (and (<= indent 3) (scan "^ {0,3}`{3,}" content))
        (-> 
         (multiple-value-list (scan-to-strings "^( {0,3})(`{3,})\\s*([^`\\s]*)(?:\\s[^`]*)?$" content))
         (second)))))

(defun is-tilde-fenced-code-block-line (line offset)
  (multiple-value-bind (indent content) (get-indented-depth-and-line line offset)
    (if (and (<= indent 3) (scan "^ {0,3}~{3,}" content))
        (-> 
         (multiple-value-list (scan-to-strings "^( {0,3})(~{3,})\\s*([^\\s]*)(?:\\s.*)?$" content))
         (second)))))

(defun attach-fenced-code-block!? (node line offset)
  (when (or (is-backtick-fenced-code-block-line line offset)
            (is-tilde-fenced-code-block-line line offset))
    (let ((child (make-fenced-code-block-node line offset)))
      (add-child node child)
      child)))

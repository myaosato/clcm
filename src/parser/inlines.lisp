(defpackage :clcm/parser/inlines
  (:use :cl)
  (:export :make-inline-parser
           ; parser handlers
           :push-parsed
           :push-delimiter
           :search-delimiter
           :remove-delimiter
           :remove-delimiters-bwtween
           :remove-all-delimiters-above
           :update-pointer
           :inactivate-before
           ; parser slots
           :parser-lines
           :parser-inlines
           :parser-delimiters-bottom
           ; delimiter slots
           :delimiter-is-active
           :delimiter-type
           :delimiter-content
           :delimiter-next
           :delimiter-prev
           :delimiter-opener-p
           :delimiter-closer-p
           :delimiter-length
           :delimiter-pointer
           :delimiter-pointer-prev))
(in-package :clcm/parser/inlines)

; delimiter stack
(defclass delimiter ()
  ((next           :initform nil                     :accessor next :reader delimiter-next)
   (prev           :initform nil                     :accessor prev :reader delimiter-prev)
   (is-active      :initform t                       :accessor delimiter-is-active)
   (delimiter-type :initarg :type                    :accessor delimiter-type)
   (pointer        :initarg :pointer                 :accessor delimiter-pointer)
   (pointer-prev   :initarg :pointer-prev            :accessor delimiter-pointer-prev)
   (number-of      :initarg :number-of :initform 1   :accessor delimiter-length)
   (is-opener      :initarg :is-opener :initform t   :reader   delimiter-opener-p)
   (is-closer      :initarg :is-closer :initform nil :reader   delimiter-closer-p)))

(defun delimiter-content (delimiter)
  ;;
  ;; (delimiter-pointer delimiter) := ("[" "foo" ... )
  ;;                                  or ("![" "foo" ... )
  (cdr (delimiter-pointer delimiter)))

; inline parser
(defclass inline-parser ()
  ((lines     :initarg :lines      :accessor parser-lines)
   (inlines   :initarg :inlines    :accessor parser-inlines)
   (pointer   :initarg :pointer    :accessor parser-pointer)
   (delimters :initarg :delimiters :accessor parser-delimiters)
   (bottom    :initarg :bottom     :accessor parser-delimiters-bottom)))

(defun make-inline-parser (lines)
  (let ((inlines (list nil)))
    (make-instance 'inline-parser
                   :lines lines
                   :inlines inlines
                   :pointer inlines
                   :delimiters nil
                   :bottom nil)))

;; push as parsed
(defun push-parsed (parser parsed)
  (let ((pointer (list parsed)))
    (setf (cdr (parser-pointer parser)) pointer)
    (setf (parser-pointer parser) pointer)))

;; push as parsed and push as delimiter
(defun push-delimiter (parser parsed type &key (number-of 1) (is-opener t) (is-closer nil))
  (let ((delimiter (make-instance 'delimiter
                                  :pointer-prev (parser-pointer parser)
                                  :pointer (push-parsed parser parsed)
                                  :type type
                                  :number-of number-of :is-opener is-opener :is-closer is-closer))
        (stack (parser-delimiters parser)))
    (when stack
      (setf (prev delimiter) stack)
      (setf (next stack) delimiter))
    (unless (parser-delimiters-bottom parser)
      (setf (parser-delimiters-bottom parser) delimiter))
    (setf (parser-delimiters parser) delimiter)))

;; return first found delimiter that has one of specified types.
(defun search-delimiter (parser &rest types)
  (labels ((%search-delimiter (delimiter &rest types)
             (if (and delimiter (find (delimiter-type delimiter) types))
                 delimiter
                 (apply #'%search-delimiter (prev delimiter) types))))
      (apply #'%search-delimiter (parser-delimiters parser) types)))

;;
(defun remove-delimiter (parser delimiter)
  (let ((prev (prev delimiter))
        (next (next delimiter)))
    (when prev (setf (next prev) next))
    (when next (setf (prev next) prev)))
  (when (eq delimiter (parser-delimiters parser))
    (setf (parser-delimiters parser) (prev delimiter)))
  (when (eq delimiter (parser-delimiters-bottom parser))
    (setf (parser-delimiters-bottom parser) nil)))

(defun remove-delimiters-between (parser opener closer)
  (unless (eq (next opener) closer)
    (let ((prev (prev closer)))
      (remove-delimiter parser prev)
      (remove-delimiters-between parser opener prev))))

(defun remove-all-delimiters-above (parser delimiter)
  (cond (delimiter
         (setf (next delimiter) nil))
        (t
         (setf (parser-delimiters parser) nil)
         (setf (parser-delimiters-bottom parser) nil))))

;; update parser pointer with delimiter pointer
(defun update-pointer (parser pointer)
  (setf (parser-pointer parser) pointer))

;;
(defun inactivate-before (delimiter &rest types)
  (when delimiter
    (when (find (delimiter-type delimiter) types)
      (setf (delimiter-is-active delimiter) nil))
    (apply #'inactivate-before (prev delimiter) types)))

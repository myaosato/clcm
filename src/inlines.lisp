(defpackage :clcm/inlines
  (:use :cl)
  (:import-from :clcm/code-spans
                :check-code-spans)
  (:import-from :clcm/autolinks
                :check-autolinks)
  (:import-from :clcm/line-breaks
                :check-line-breaks)
  (:import-from :clcm/raw-html
                :check-raw-html)
  (:import-from :clcm/backslash-escapes
                :check-backslash-escapes)
  (:import-from :clcm/links
                :check-link-opener
                :check-image-opener
                :check-link-closer
                :check-close-link-text)
  (:export :parse-inline))
(in-package :clcm/inlines)

; delimiter stack
(defclass delimiter ()
  ((next :accessor next :initform nil)
   (prev :accessor prev :initform nil)
   (is-active :accessor is-active :initform t)
   (delimiter-type :accessor delimiter-type :initarg :type)
   (pointer :accessor pointer :initarg :pointer)
   (number-of :accessor number-of :initarg :number-of :initform 1)
   (is-opener :accessor is-opener :initarg :is-opener :initform t)
   (is-closer :accessor is-closer :initarg :is-closer :initform nil)))

(defun push-delimiter (stack type pointer &key (number-of 1) (is-opener t) (is-closer nil))
  (let ((delimiter (make-instance 'delimiter
                                  :pointer pointer :type type
                                  :number-of number-of :is-opener is-opener :is-closer is-closer)))
    (unless stack
      (return-from push-delimiter delimiter))
    (setf (prev delimiter) stack)
    (setf (next stack) delimiter)
    delimiter))

(defun search-delimiter (delimiter &rest types)
  (when delimiter
    (if (find (delimiter-type delimiter) types)
        delimiter
        (apply #'search-delimiter (prev delimiter) types))))

(defun remove-delimiter (delimiter)
  (let ((prev (prev delimiter))
        (next (next delimiter)))
    (when prev (setf (next prev) next))
    (when next (setf (prev next) prev))))

(defun inactivate-before (delimiter &rest types)
  (when delimiter
    (when (find (delimiter-type delimiter) types)
      (setf (is-active delimiter) nil))
    (apply #'inactivate-before (prev delimiter) types)))

; inline parser
(defclass inline-parser ()
  ((lines :initarg :lines :accessor parser-lines)
   (inlines :initarg :inlines :accessor parser-inlines)
   (pointer :initarg :pointer :accessor parser-pointer)
   (delimter-stack :initarg :delimiter-stack :accessor parser-delimiters)))

(defun make-inline-parser (lines)
  (let ((inlines (list nil)))
    (make-instance 'inline-parser
                   :lines lines
                   :inlines inlines
                   :pointer inlines
                   :delimiter-stack nil)))

(defun push-parsed (parser target)
  (let ((previous (parser-pointer parser)))
    (setf (cdr (parser-pointer parser)) (list target))
    (setf (parser-pointer parser) (cdr (parser-pointer parser)))
    previous))

(defun push-to-parser-delimiters (parser parsed type &key (number-of 1) (is-opener t) (is-closer nil))
  (let ((pushed (push-parsed parser parsed)))
    (setf (parser-delimiters parser)
          (push-delimiter (parser-delimiters parser) type pushed
                          :number-of number-of :is-opener is-opener :is-closer is-closer))))

;; return next position (on lines)
(defun process-result (parser pos type parsed next)
  (cond ((eq type :link-opener)
         (push-to-parser-delimiters parser parsed :link))
        ((eq type :image-opener)
         (push-to-parser-delimiters parser parsed :image))
        ((eq type :link-closer)
         (let ((opener (search-delimiter (parser-delimiters parser) :link :image)))
           (cond ((and opener (is-active opener))
                  (let ((closer (check-link-closer (parser-lines parser) (1+ pos))))
                    (if closer
                        (destructuring-bind (_ (dest title) next-pos) closer
                          (declare (ignore _))
                          (let ((type (delimiter-type opener))
                                (content (cddr (pointer opener))))
                            ;; (pointer opener) := (previous-last "[" "foo" ... ) 
                            ;;                     or ((previous-last "![" "foo" ... )
                            (setf (parser-pointer parser) (pointer opener))
                            (push-parsed parser (list type content dest title))
                            (remove-delimiter opener)
                            (when (eq opener (parser-delimiters parser))
                              (setf (parser-delimiters parser) (prev opener)))
                            (when (eq type :link)
                              (inactivate-before opener :link))
                            (setf next next-pos)))
                        (push-parsed parser parsed))))
                 ((and opener (not (is-active opener)))
                  (remove-delimiter opener)
                  (when (eq opener (parser-delimiters parser))
                    (setf (parser-delimiters parser) (prev opener)))
                  (push-parsed parser parsed))
                 (t (push-parsed parser parsed)))))
        (t (push-parsed parser parsed)))
  next)

(defun parse-inline (lines)
  (let* ((parser (make-inline-parser lines)))
    (loop :with done := 0
          :with pos := 0
          :with lines := (parser-lines parser)
          :while (< pos (length lines))
          :for result := (or (check-backslash-escapes lines pos)
                             (check-code-spans lines pos)
                             (check-autolinks lines pos)
                             (check-line-breaks lines pos)
                             (check-raw-html lines pos)
                             (check-link-opener lines pos)
                             (check-image-opener lines pos)
                             (check-close-link-text lines pos))
          :if result
          :do (destructuring-bind (type parsed next) result
                (when (/= pos done)
                  (push-parsed parser (subseq lines done pos)))
                (let ((next (process-result parser pos type parsed next)))
                  (setf done next)
                  (setf pos next)))
          :else
          :do (incf pos)
          :finally (when (/= pos done)
                     (push-parsed parser (subseq lines done pos))))
    (values (cdr (parser-inlines parser)) (parser-delimiters parser))))

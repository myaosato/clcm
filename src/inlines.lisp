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
                :check-link-closer)
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

; inline parser 
(defun parse-inline (lines)
  (let* ((inlines (list nil))
         (pointer inlines)
         (delimiter-stack nil))
    (loop :with done := 0
          :with pos := 0
          :while (< pos (length lines))
          :for result := (or (check-backslash-escapes lines pos)
                             (check-code-spans lines pos)
                             (check-autolinks lines pos)
                             (check-line-breaks lines pos)
                             (check-raw-html lines pos)
                             (check-link-opener lines pos)
                             (check-link-closer lines pos))
          :if result
          :do (destructuring-bind (type parsed next) result
                (when (/= pos done)
                  (setf (cdr pointer) (list (subseq lines done pos)))
                  (setf pointer (cdr pointer)))
                (cond ((eq type :link-opener)
                       (setf (cdr pointer) (list parsed))
                       (setf delimiter-stack
                             (push-delimiter delimiter-stack :link (cdr pointer)))
                       (setf pointer (cdr pointer)))
                      ((eq type :link-closer)
                       )
                      (t
                       (setf (cdr pointer) (list parsed))
                       (setf pointer (cdr pointer))))
                (setf done next)
                (setf pos next))
          :else
          :do (incf pos)
          :finally (when (/= pos done)
                     (setf (cdr pointer) (list (subseq lines done pos)))
                     (setf pointer (cdr pointer))))
    (values (cdr inlines) delimiter-stack)))

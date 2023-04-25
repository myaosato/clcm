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
        (search-delimiter (prev delimiter) types))))

(defun remove-delimiter (delimiter)
  (let ((prev (prev delimiter))
        (next (next delimiter)))
    (when prev (setf (next prev) next))
    (when next (setf (prev next) prev))))

(defun inactivate-before (delimiter &rest types)
  (when delimiter
    (when (find (delimiter-type delimiter) types)
      (setf (is-active delimiter) nil))
    (inactivate-before (prev delimiter))))

; inline parser
(defmacro push-parsed (pointer target)
  `(let ((previous pointer))
     (setf (cdr ,pointer) (list ,target))
     (setf ,pointer (cdr ,pointer))
     previous))

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
                             (check-image-opener lines pos)
                             (check-close-link-text lines pos))
          :if result
          :do (destructuring-bind (type parsed next) result
                (when (/= pos done)
                  (push-parsed pointer (subseq lines done pos)))
                (cond ((eq type :link-opener)
                       (let ((pushed (push-parsed pointer parsed)))
                       (setf delimiter-stack
                             (push-delimiter delimiter-stack :link pushed))))
                      ((eq type :image-opener)
                       (let ((pushed (push-parsed pointer parsed)))
                       (setf delimiter-stack
                             (push-delimiter delimiter-stack :image pushed))))
                      ((eq type :link-closer)
                       (let ((opener (search-delimiter delimiter-stack :link :image)))
                         (cond ((and opener (is-active opener))
                                (let ((closer (check-link-closer lines (1+ pos))))
                                  (if closer
                                      (destructuring-bind (_ (dest title) next-pos) closer
                                        (declare (ignore _))
                                        (let ((type (delimiter-type opener))
                                              (content (cddr (pointer opener))))
                                          ;; (pointer opener) := ("[" "foo" ... ) or ("![" "foo" ... )
                                          (setf pointer (pointer opener))
                                          (push-parsed pointer (list type content dest title))
                                          (remove-delimiter opener)
                                          (when (eq type :link)
                                            ; TODO inactivate
                                            (inactivate-before opener :link))
                                          (setf next next-pos)))
                                      (push-parsed pointer parsed))))
                               ((and opener (not (is-active opener)))
                                (remove-delimiter opener)
                                (push-parsed pointer parsed))
                               (t
                                (push-parsed pointer parsed)))))
                      (t
                       (push-parsed pointer parsed)))
                (setf done next)
                (setf pos next))
          :else
          :do (incf pos)
          :finally (when (/= pos done)
                     (push-parsed pointer (subseq lines done pos))))
    (values (cdr inlines) delimiter-stack)))

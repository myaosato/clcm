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

(defun parse-inline (lines)
  (let* ((inlines (list nil))
         (pointer inlines)
         (opener-stack nil))
    (loop :with done := 0
          :with pos := 0
          :while (< pos (length lines))
          :for result := (or (check-backslash-escapes lines pos)
                             (check-code-spans lines pos)
                             (check-autolinks lines pos)
                             (check-line-breaks lines pos)
                             (check-raw-html lines pos)
                             (check-link-opener lines pos))
          :if result
          :do (destructuring-bind (parsed . next) result
                (when (/= pos done)
                  (setf (cdr pointer) (list (subseq lines done pos)))
                  (setf pointer (cdr pointer)))
                (cond ((eq (car parsed) :link-opener)
                       (setf (cdr pointer) (list (cadr parsed)))
                       (push (list (cdr pointer) :link t) opener-stack)
                       (setf pointer (cdr pointer)))
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
    (values (cdr inlines) opener-stack)))

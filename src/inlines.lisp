(defpackage :clcm/inlines
  (:use :cl)
  (:export :inlines->html))
(in-package :clcm/inlines)

(defun inlines->html (strings &key last-break)
  (if (null strings) (return-from inlines->html ""))
  (format nil "~{~A~^~%~}~A" strings (if last-break #\Newline "")))
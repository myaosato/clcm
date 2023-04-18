(defpackage :clcm/backslash-escapes
  (:use :cl)
  (:import-from :cl-ppcre
                :scan-to-strings)
  (:export :check-backslash-escapes))
(in-package :clcm/backslash-escapes)

(defun check-backslash-escapes (lines pos)
  (let ((escaped (and (scan-to-strings "^\\\\" lines :start pos)
                      (scan-to-strings "^[-!\"#$%&'()*+,.:;<=>?@[\\]^_`{|}~\\\\]" lines :start (1+ pos)))))
    (when escaped
      (list :backslash-escape (list :text escaped) (+ pos 2)))))

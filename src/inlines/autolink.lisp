(defpackage :clcm/inlines/autolink
  (:use :cl
        :clcm/inlines/parser)
  (:export :scan-autolink))
(in-package :clcm/inlines/autolink)

(defun scan-autolink (parser)
  (or (scan-link parser)
      (scan-mail parser)))

(defun scan-link (parser)
  (multiple-value-bind (result matches)
      (scan-to-strings parser "^<([A-Za-z][A-Za-z0-9+.-]{1,32}:[^> ]*)>")
    (when result
      (push-string parser (format nil "<a href=\"~A\">~A</a>" (aref matches 0) (aref matches 0)))
      (pos+ parser (length result)))))

(defun scan-mail (parser)
  (let ((regex "^<([a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*)>"))
  (multiple-value-bind (result matches)
      (scan-to-strings parser regex)
    (when result
      (push-string parser (format nil "<a href=\"mailto:~A\">~A</a>" (aref matches 0) (aref matches 0)))
      (pos+ parser (length result))))))

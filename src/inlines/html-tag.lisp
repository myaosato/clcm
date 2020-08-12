(defpackage :clcm/inlines/html-tag
  (:use :cl
        :clcm/raw-html-regex
        :clcm/inlines/parser)
  (:export :scan-html-tag))
(in-package :clcm/inlines/html-tag)

(defun scan-html-tag (parser)
  (scan&push parser *html-tag*))

(defpackage :clcm/clcm
  (:nicknames :clcm)
  (:use :cl)
  (:import-from :clcm/inlines)
  (:export :cm->html))

(in-package :clcm/clcm)

(defun cm->html (cm-string)
  ; convert from CommonMark string to HTML string
  "")

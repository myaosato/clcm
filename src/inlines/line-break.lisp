(defpackage :clcm/inlines/line-break
  (:use :cl
        :clcm/inlines/parser)
  (:export :scan-line-break))
(in-package :clcm/inlines/line-break)

(defun scan-line-break (parser)
  (or (scan&+ parser '(:sequence :start-anchor (:greedy-repetition 2 nil #\Space) :end-anchor))
      (and (scan&+ parser '(:sequence :start-anchor (:greedy-repetition 2 nil #\Space) #\Newline))
           (push-string parser (format nil "<br />~%")))
      (and (scan&+ parser '(:sequence :start-anchor #\Space #\Newline))
           (push-string parser (format nil "~%")))))

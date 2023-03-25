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
  (:export :parse-inline))
(in-package :clcm/inlines)

(defun parse-inline (lines)
  (let ((inlines nil))
    (loop :with done := 0
          :with pos := 0
          :while (< pos (length lines))
          :for result := (or (check-backslash-escapes lines pos)
                             (check-code-spans lines pos)
                             (check-autolinks lines pos)
                             (check-line-breaks lines pos)
                             (check-raw-html lines pos))
          :if result
          :do (destructuring-bind (parsed . next) result
                (when (/= pos done)
                  (push (subseq lines done pos) inlines))
                (push parsed inlines)
                (setf done next)
                (setf pos next))
          :else
          :do (incf pos))
    (reverse inlines)))

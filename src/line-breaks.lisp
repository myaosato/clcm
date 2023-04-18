(defpackage :clcm/line-breaks
  (:use :cl)
  (:import-from :cl-ppcre
                :scan-to-strings)
  (:export :check-line-breaks))
(in-package :clcm/line-breaks)

(defun check-line-breaks (lines pos)
  (let* ((re-hard-line-break "^(?:\\\\| {2,})[\\n\\r]")
         (re-soft-line-break "^ ?[\\n\\r]")
         (hard-line-break (scan-to-strings re-hard-line-break lines :start pos))
         (soft-line-break (or hard-line-break
                              (scan-to-strings re-soft-line-break lines :start pos))))
    (cond (hard-line-break
           (list :hard-line-break :hard-line-break (+ pos (length hard-line-break))))
          (soft-line-break
           (list :soft-line-break :soft-line-break (+ pos (length soft-line-break))))
          (t nil))))

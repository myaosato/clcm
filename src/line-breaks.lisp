(defpackage :clcm/line-breaks
  (:use :cl)
  (:import-from :cl-ppcre
                :scan-to-strings)
  (:export :check-line-breaks))
(in-package :clcm/line-breaks)

(defun check-line-breaks (line pos)
  (let* ((re-hard-line-break "^(?:\\\\| {2,})[\\n\\r]"))
         (re-soft-line-break "^ ?[\\n\\r]"))
         (hard-line-break (scan-to-strings re-hard-line-break line :start pos))
         (soft-line-break (or hard-line-break
                              (scan-to-strings re-soft-line-break line :start pos))))
    (cond (hard-line-break
           (cons :hard-line-break (+ pos (length hard-line-break))))
          (soft-line-break
           (cons :soft-line-break (+ pos (length soft-line-break))))
          (t
           (cons nil pos)))))

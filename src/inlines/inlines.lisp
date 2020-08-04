(defpackage :clcm/inlines/inlines
  (:use :cl
        :clcm/utils
        :clcm/raw-html-regex
        :clcm/characters
        :clcm/inlines/parser
        :clcm/inlines/backslash-escape
        :clcm/inlines/code-span
        :clcm/inlines/special-characters
        :clcm/inlines/html-tag
        :clcm/inlines/line-break
        :clcm/inlines/emphasis)
  (:import-from :cl-ppcre)
  (:export :inlines->html
           :inlines->html*))
(in-package :clcm/inlines/inlines)

;; api
(defun inlines->html (strings &key last-break)
  (%inlines->html strings
                  (lambda (parser)
                    (or (scan-backslash-escape parser)
                        (scan-code-span parser #'inlines->html*)
                        (scan-html-tag parser)
                        (scan-line-break parser)
                        (scan-emphasis parser)
                        (scan-special-characters parser)
                        (push-chars parser (read-c parser))))
                  last-break))

(defun inlines->html* (strings &key last-break)
  ;; only
  ;; < -> &lt;
  ;; > -> &gt;
  ;; & -> &amp;
  ;; double quote -> &quot;
  (%inlines->html strings
                  (lambda (parser)
                    (or (scan-special-characters parser)
                        (push-chars parser (read-c parser))))
                  last-break))

;; main functions
(defun %inlines->html (strings proc last-break)
  (if (null strings) (return-from %inlines->html ""))
  (let* ((chars (format nil "~{~A~^~%~}~A" strings (if last-break #\Newline "")))
         (parser (make-inlines-parser :input chars)))
      (loop :while (peek-c parser)
            :do (funcall proc parser))
    (output parser)))

(defun output (parser)
  (format nil "~A" (ip-queue parser)))

(defpackage :clcm/autolinks
  (:use :cl)
  (:import-from :cl-ppcre
                :scan-to-strings)
  (:export :check-autolinks))
(in-package :clcm/autolinks)

(defun check-autolinks (lines pos)
  (let* ((re-uri "^<[A-Za-z][A-Za-z0-9+.-]{1,31}:[^\\x00-\\x1F\\x7F <>]+>")
         (re-email (format nil "^<~A@~A~A>"
                           "[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+"
                           "[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?"
                           "(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*"))
         (uri-link (scan-to-strings re-uri lines :start pos))
         (uri-link-length (length uri-link))
         (email-link (scan-to-strings re-email lines :start pos))
         (email-link-length (length email-link)))
    (cond (email-link
           (let ((email (subseq email-link 1 (1- email-link-length))))
             (list :autolink (list :autolink (format nil "mailto:~A" email) email) (+ pos email-link-length))))
          (uri-link
           (let ((uri (subseq uri-link 1 (1- uri-link-length))))
             (list :autolink (list :autolink uri uri) (+ pos uri-link-length))))
          (t nil))))

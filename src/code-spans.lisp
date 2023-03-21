(defpackage :clcm/code-spans
  (:use :cl)
  (:import-from :cl-ppcre
                :scan-to-strings)
  (:export :check-code-spans))
(in-package :clcm/code-spans)

(defun check-code-spans (lines pos)
  (let* ((backticks (scan-to-strings "^`+" lines :start pos))
         (re-code  (format nil "[\\w\\W]+?(?=~A(?:[^`]|$))" backticks))
         (content (and backticks (scan-to-strings re-code lines :start (+ pos (length backticks)))))
         (content-length (length content))
         (backtick-length (length (and content backticks))))
    (setf content (and content (map 'string
                                    (lambda (x) (if (or (char= #\linefeed x) (char= #\return x)) #\space x))
                                    content)))
    (when (scan-to-strings "^ .*[^ ].* $" content)
      (setf content (subseq content 1 (1- (length content)))))
    (cons (and content (list :code content)) (+ pos (* backtick-length 2) content-length))))


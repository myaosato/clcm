(defpackage :clcm/inlines
  (:use :cl)
  (:export :inlines->html))
(in-package :clcm/inlines)

;; api
(defun inlines->html (strings &key last-break)
  (if (null strings) (return-from inlines->html ""))
  (let ((chars (format nil "~{~A~^~%~}~A" strings (if last-break #\Newline ""))))
    (chars->html chars)))

;; parser
(defstruct (inlines-parser (:conc-name ip-))
  (input "")
  (stack nil)
  (position 0))

(defun chars->html (chars)
  (let ((parser (make-inlines-parser :input chars)))
    (run parser)
    (output parser)))

(defun read-c (parser)
  (let ((pos (ip-position parser))
        (input (ip-input parser)))
  (when (< pos (length input))
    (let ((char (char input pos)))
      (incf (ip-position parser))
      char))))

(defun peek-c (parser &optional (n 0))
  (let ((pos (+ (ip-position parser) n))
        (input (ip-input parser)))
  (when (< pos (length input))
    (char input pos))))

(defun push-chars (parser &rest chars)
  (loop :for c :in chars
        :do (push c (ip-stack parser))))

(defun push-string (parser string)
  (loop :for c :across string
        :do (push c (ip-stack parser))))

;; main function 
(defun run (parser)
  (cond ((null (peek-c parser)) ;; END
         (output parser))
        ((and (is-backslash (peek-c parser)) (is-ascii-punctuation-character (peek-c parser 1)))
         (read-c parser)
         (push-chars parser (read-c parser))
         (run parser))
        ((and (is-backslash (peek-c parser)) (is-eol (peek-c parser 1)))
         (read-c parser)
         (push-string parser "<br />")
         (push-chars parser (read-c parser))
         (run parser))
        (t
         (push-chars parser (read-c parser))
         (run parser))))

(defun output (parser)
  (concatenate 'string (reverse (ip-stack parser))))

;; 

;;
(defun is-backslash (char)
  (and char (char= char #\\)))

(defun is-ascii-punctuation-character (char)
  (and char
       (let ((code (char-code char)))
         (or (<= #x21 code #x2f)
             (<= #x3a code #x40)
             (<= #x5b code #x60)
             (<= #x7b code #x7e)))))

(defun is-eol (char)
  (and char (char= char #\Newline)))



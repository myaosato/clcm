(defpackage :clcm/inlines
  (:use :cl)
  (:import-from :cl-ppcre)
  (:export :inlines->html))
(in-package :clcm/inlines)

;; api
(defun inlines->html (strings &key last-break)
  (if (null strings) (return-from inlines->html ""))
  (let ((chars (format nil "~{~A~^~%~}~A" strings (if last-break #\Newline ""))))
    (chars->html chars)))

;; vars
(defvar *ascii-punctuations*
  '(#\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+ #\, #\- #\. #\/
    #\: #\; #\< #\= #\> #\? #\@
    #\[ #\\ #\] #\^ #\_ #\`
    #\{ #\| #\} #\~))

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

(defun scan (parser regex)
  (cl-ppcre:scan regex (ip-input parser) :start (ip-position parser)))

(defun pos+ (parser &optional (n 1))
  (incf (ip-position parser) n))

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
        ((scan parser `(:sequence :start-anchor #\\ (:char-class ,@*ascii-punctuations*)))
         (pos+ parser)
         (push-chars parser (read-c parser))
         (run parser))
        ((scan parser '(:sequence :start-anchor #\\ #\Newline))
         (pos+ parser 2)
         (push-string parser (format nil "<br />~%"))
         (run parser))
        ;;
        ((scan parser "^<")
         (pos+ parser)
         (push-string parser "&lt;")
         (run parser))
        ((scan parser "^>")
         (pos+ parser)
         (push-string parser "&gt;")
         (run parser))
        ((scan parser "^&")
         (pos+ parser)
         (push-string parser "&amp;")
         (run parser))
        ;; 
        (t
         (push-chars parser (read-c parser))
         (run parser))))

(defun output (parser)
  (concatenate 'string (reverse (ip-stack parser))))

;; 

;;
(defun is-backslash (char)
  (and char (char= char #\\)))

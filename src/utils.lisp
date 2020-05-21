(defpackage :clcm/utils
  (:use :cl)
  (:export :->
           :->>
           :last-char
           :trim-left-space-max-n))
(in-package :clcm/utils)

;; https://gist.github.com/myaosato/98d08623768e53af10f0da9810b7eb3f
(defmacro -> (prev &rest rest)
  (if (null rest)         
      prev         
      `(-> ,(cons (caar rest) (cons prev (cdar rest))) ,@(cdr rest))))

(defmacro ->> (prev &rest rest)
  (if (null rest)
      prev       
      `(->> ,(append (car rest) (list prev)) ,@(cdr rest))))

;; 
(defun last-char (string)
  (char string (1- (length string))))

(defun repeat-char (char length)
  (if (> length 0)
      (make-array (list length) :element-type 'character :initial-element char)
      ""))

(defun trim-left-space-max-n (string n)
  (cond ((<= n 0)
         string)
        ((string= string "")
         string)
        ((char= (char string 0) #\Space)
         (trim-left-space-max-n (subseq string 1) (1- n)))
        (t string)))

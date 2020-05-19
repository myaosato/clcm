(defpackage :clcm/utils
  (:use :cl)
  (:export :->
           :->>
           :last-char))
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

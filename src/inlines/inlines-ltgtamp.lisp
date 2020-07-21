(defpackage :clcm/inlines/inlines-ltgtamp
  (:use :cl)
  (:export :<>&->ref))
(in-package :clcm/inlines/inlines-ltgtamp)

;; api
(defun <>&->ref (strings &key last-break)
  (if (null strings) (return-from <>&->ref ""))
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
  (let ((c (read-c parser)))
    (if (null c) (return-from run (output parser)))
    (cond ((char= c #\")
           (push-string parser "&quot;"))
          ((char= c #\<)
           (push-string parser "&lt;"))
          ((char= c #\>)
           (push-string parser "&gt;"))
          ((char= c #\&)
           (push-string parser "&amp;"))
          (t
           (push-chars parser c)))
    (run parser)))
  
(defun output (parser)
  (concatenate 'string (reverse (ip-stack parser))))

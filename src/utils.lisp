(defpackage :clcm/utils
  (:use :cl)
  (:export :->
           :->>
           :last-char
           :trim-left-space-max-n
           :repeat-char
           :replace&adjust
           :*string-tab*))
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
;;
(defvar *string-tab* (format nil "~A" #\Tab))

;; adjustable vector
(defun replace&adjust (sequence1 sequence2 &key (start1 0) end1 (start2 0) end2)
   (let* ((source (subseq sequence2 start2 end2))
          (end1 (or end1 (length sequence1)))
          (delta (- (length source) (- end1 start1)))
          (rest (subseq sequence1 end1)))
     (if (< 0 delta)
         (dotimes (_ delta) (vector-push-extend #\Null sequence1))
         (dotimes (_ (- delta)) (vector-pop sequence1)))
     (replace sequence1 source :start1 start1)
     (replace sequence1 rest :start1 (+ start1 (length source)))
     sequence1))

(defpackage :clcm/inlines/parser
  (:use :cl
        :clcm/utils)
  (:import-from :cl-ppcre)
  (:export :make-inlines-parser
           :ip-queue
           :ip-stack
           :ip-position
           :read-c
           :peek-c
           :scan
           :scan-to-strings
           :scan&push
           :scan&+
           :pos+
           :push-chars
           :push-string
           :replace-string
           :make-inline-op
           :iop-type
           :iop-start
           :iop-end
           :iop-num
           :push-op
           :pop-op))
(in-package :clcm/inlines/parser)

;;;; parser
(defstruct (inlines-parser (:conc-name ip-))
  (input "")
  (queue (make-array 100 :element-type 'character :fill-pointer 0 :adjustable t))
  (stack nil)
  (position 0))

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

(defun scan (parser regex &key (offset 0))
  (cl-ppcre:scan regex (ip-input parser) :start (+ (ip-position parser) offset)))

(defun scan-to-strings (parser regex &key (offset 0))
  (cl-ppcre:scan-to-strings regex (ip-input parser) :start (+ (ip-position parser) offset)))

(defun scan&push (parser regex)
  (let ((result (scan-to-strings parser regex)))
    (when result
      (pos+ parser (length result))
      (push-string parser result))))

(defun scan&+ (parser regex)
  (let ((result (scan-to-strings parser regex)))
    (when result
      (pos+ parser (length result)))))

(defun pos+ (parser &optional (n 1))
  (incf (ip-position parser) n))

(defun push-chars (parser &rest chars)
  (loop :for c :in chars
        :do (vector-push-extend c (ip-queue parser)))
  chars)

(defun push-string (parser string)
  (loop :for c :across string
        :do (vector-push-extend c (ip-queue parser)))
  string)

(defun replace-string (parser string start end)
  (replace&adjust (ip-queue parser) string :start1 start :end1 end))

;; op stack TODO
(defstruct (inline-op (:conc-name iop-))
  (type "")
  (start 0)
  (end 0)
  (num 0))

(defun push-op (parser iop)
  (push iop (ip-stack parser)))

(defun pop-op (parser type)
  (%pop-op parser type (ip-stack parser)))

(defun %pop-op (parser type stack)
  (cond ((null stack)
         nil)
        ((string= (iop-type (car stack)) type)
         (let ((op (car stack)))
           (setf (ip-stack parser) (cdr stack))
           op))
        (t
         (%pop-op parser type (cdr stack)))))

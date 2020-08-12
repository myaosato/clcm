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
           :make-delimiter
           :dl-type
           :dl-open
           :dl-close
           :dl-start
           :dl-num
           :shift-position
           :push-op))
(in-package :clcm/inlines/parser)

;; op stack
(defstruct (delimiter (:conc-name dl-))
  (type nil)
  (open nil)
  (close nil)
  (start 0)
  (num 0))

(defun shift-position (delimiter offset)
  (incf (dl-start delimiter) offset))

;; parser
(defstruct (inlines-parser (:conc-name ip-))
  (input "")
  (queue (make-array 100 :element-type 'character :fill-pointer 0 :adjustable t))
  (stack (make-array 100 :element-type 'delimiter :fill-pointer 0 :adjustable t))
  (position 0))

;; read & peek
(defun read-c (parser)
  (let ((pos (ip-position parser))
        (input (ip-input parser)))
  (when (< -1 pos (length input))
    (let ((char (char input pos)))
      (incf (ip-position parser))
      char))))

(defun peek-c (parser &optional (n 0))
  (let ((pos (+ (ip-position parser) n))
        (input (ip-input parser)))
  (when (< -1 pos (length input))
    (char input pos))))

;; scan
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


;; output queue
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

;; op stack
(defun push-op (parser iop)
  (vector-push-extend iop (ip-stack parser)))

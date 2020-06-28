(defpackage :clcm/inlines/inlines
  (:use :cl
        :clcm/utils
        :clcm/raw-html-regex
        :clcm/inlines/inlines-ltgtamp)
  (:import-from :cl-ppcre)
  (:export :inlines->html))
(in-package :clcm/inlines/inlines)

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

;;;; parser
(defstruct (inlines-parser (:conc-name ip-))
  (input "")
  (queue (make-array 100 :element-type 'character :fill-pointer 0 :adjustable t))
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

(defun scan (parser regex &key (offset 0))
  (cl-ppcre:scan regex (ip-input parser) :start (+ (ip-position parser) offset)))

(defun scan-to-strings (parser regex)
  (cl-ppcre:scan-to-strings regex (ip-input parser) :start (ip-position parser)))

(defun scan&push (parser regex)
  (let ((result (scan-to-strings parser regex)))
    (when result
      (pos+ parser (length result))
      (push-string parser result)
      t)))

(defun scan&+ (parser regex)
  (let ((result (scan-to-strings parser regex)))
    (when result
      (pos+ parser (length result))
      t)))

(defun pos+ (parser &optional (n 1))
  (incf (ip-position parser) n))

(defun push-chars (parser &rest chars)
  (loop :for c :in chars
        :do (vector-push-extend c (ip-queue parser))))

(defun push-string (parser string)
  (loop :for c :across string
        :do (vector-push-extend c (ip-queue parser))))

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

;;;;

;; main function
(defun run (parser)
  (cond ((null (peek-c parser)) ;; END
         (output parser))
        ((scan\-escape parser))
        ((scan-code-span parser))
        ((scan-html-tag parser))
        ((scan-line-break parser))
        ((scan<>& parser))
        (t ;; read-char
         (push-chars parser (read-c parser))
         (run parser))))

(defun scan-html-tag (parser)
  (if (scan&push parser *html-tag*)
      (run parser)))

(defun scan-line-break (parser)
  (cond ((scan&+ parser '(:sequence :start-anchor (:greedy-repetition 2 nil #\Space) :end-anchor))
         (run parser))
        ((scan&+ parser '(:sequence :start-anchor (:greedy-repetition 2 nil #\Space) #\Newline))
         (push-string parser (format nil "<br />~%"))
         (run parser))
        ((scan&+ parser '(:sequence :start-anchor #\Space #\Newline))
         (push-string parser (format nil "~%"))
         (run parser))))

(defun scan\-escape (parser)
  (cond ((scan parser `(:sequence :start-anchor #\\ (:char-class ,@*ascii-punctuations*)))
         (pos+ parser)
         (push-chars parser (read-c parser))
         (run parser))
        ((scan parser '(:sequence :start-anchor #\\ #\Newline))
         (pos+ parser 2)
         (push-string parser (format nil "<br />~%"))
         (run parser))))

(defun scan-code-span (parser)
  (let ((start-backticks (scan-to-strings parser "^`+")))
    (when start-backticks
      (let ((target (format nil "^~A([^`]|[^`](:?.|\\n)*?[^`])~A(?:[^`]|\\n|$)"
                            start-backticks start-backticks)))
      (multiple-value-bind (result strs) (scan-to-strings parser target)
        (cond (result
               (pos+ parser (+ (* 2 (length start-backticks)) (length (aref strs 0))))
               (let ((content (aref strs 0)))
                 (loop :for i :from 0 :to (1- (length content))
                       :if (char= (aref content i) #\Newline)
                       :do (setf (aref content i) #\Space))
                 (if (and (not (cl-ppcre:scan "^ +$" content))
                          (>= (length content) 3)
                          (char= (char content 0) #\Space)
                          (char= (char content (1- (length content))) #\Space))
                     (setf content (subseq content 1 (1- (length content)))))
                 (push-string parser (format nil "<code>~A</code>" (<>&->ref (list content))))
                 (run parser)))
              (t
               (pos+ parser (length start-backticks))
               (push-string parser start-backticks)
               (run parser))))))))

(defun scan<>& (parser)
  (cond ((scan parser "^<")
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
         (run parser))))

(defun output (parser)
  (format nil "~A" (ip-queue parser)))

;;

;;
(defun is-backslash (char)
  (and char (char= char #\\)))

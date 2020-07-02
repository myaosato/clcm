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
(defvar *ascii-punctuations-without-*_*
  '(#\! #\" #\# #\$ #\% #\& #\' #\( #\) #\+ #\, #\- #\. #\/
    #\: #\; #\< #\= #\> #\? #\@
    #\[ #\\ #\] #\^ #\`
    #\{ #\| #\} #\~))

(defvar *unicode-whitespaces*
  (list (code-char #x9) (code-char #xD) (code-char #xA) (code-char #xC)
        (code-char #x20) (code-char #xA0)  (code-char #x1680) (code-char #x2000)
        (code-char #x2001) (code-char #x2002) (code-char #x2003) (code-char #x2004)
        (code-char #x2005) (code-char #x2006) (code-char #x2007) (code-char #x2008)
        (code-char #x2009) (code-char #x200A) (code-char #x202F) (code-char #x205F)
        (code-char #x3000)))

(defvar *ascii-punctuations*
  ;; TODO
  (append *ascii-punctuations-without-*_* (list #\* #\_)))

(defvar *punctuations*
  ;; TODO
  (append *ascii-punctuations-without-*_* (list #\* #\_)))

(defvar *punctuations-without-**
  ;; TODO
  (append *ascii-punctuations-without-*_* (list #\_)))

(defvar *punctuations-without-_*
  ;; TODO
  (append *ascii-punctuations-without-*_* (list #\*)))



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

(defun scan-to-strings (parser regex &key (offset 0))
  (cl-ppcre:scan-to-strings regex (ip-input parser) :start (+ (ip-position parser) offset)))

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
        ((scan-*_ parser))
        ((scan<>& parser))
        (t ;; read-char
         (push-chars parser (read-c parser))
         (run parser))))

;;
(defun scan-*_ (parser)
  (if (not (scan parser "^\\*")) (return-from scan-*_ nil))
  (when (or (close-* parser)
            (open-* parser))
    (run parser)))

(defun *-can-open (parser)
  ;; TODO
  (or/mv (scan-to-strings parser
                          `(:sequence :modeless-start-anchor
                            (:regex "(\\*+)")
                            (:inverted-char-class  ,@`,(append *unicode-whitespaces* *punctuations*))))
         (if (/= (ip-position parser) 0)
             (scan-to-strings parser
                              `(:sequence :modeless-start-anchor
                                (:char-class
                                 ,@`,(append *unicode-whitespaces* *punctuations-without-**))
                                (:regex "(\\*+)")
                                (:char-class ,@*punctuations-without-**))
                              :offset -1)
             (scan-to-strings parser
                              `(:sequence :modeless-start-anchor
                                (:regex "(\\*+)")
                                (:char-class ,@*punctuations-without-**))))))

(defun open-* (parser)
  (multiple-value-bind (result s) (*-can-open parser)
    (when result
      (let* ((d (aref s 0)) ; d is delimiter run : ex. **
             (start (fill-pointer (ip-queue parser)))
             (op (make-inline-op  :type "*" :start start :end (+ start (length d)) :num (length d))))
        (push-string parser d)
        (incf (ip-position parser) (length d))
        (push-op parser op)
        op))))

(defun *-can-close (parser)
  ;; TODO
  (if (= (ip-position parser) 0) (return-from *-can-close nil))
  (or/mv (scan-to-strings parser
                          `(:sequence :modeless-start-anchor
                            (:inverted-char-class ,@`,(append *unicode-whitespaces* *punctuations*))
                            (:regex "(\\*+)"))
                          :offset -1)
         (scan-to-strings parser
                          `(:sequence :modeless-start-anchor
                            (:char-class ,@*punctuations*)
                            (:regex "(\\*+)")
                            (:alternation
                             :modeless-end-anchor-no-newline
                             (:char-class
                              ,@`,(append *unicode-whitespaces* *punctuations-without-**))))
                          :offset -1)))

(defun close-* (parser)
  (multiple-value-bind (result s) (*-can-close parser)
    (when result
      (let* ((d (aref s 0)) ; d is delimiter run : ex. **
             (op (pop-op parser "*")))
        (cond ((null op) nil)
              ; NOTE incf do not return nil
              ((= (iop-num op) (length d))
               (_*->em-strong parser (length d) (iop-start op))
               (incf (ip-position parser) (length d)))
              ((> (iop-num op) (length d))
               (let ((remained (- (iop-num op) (length d))))
                 (_*->em-strong parser (length d) (+ (iop-start op) remained))
                 (setf (iop-num op) remained)
                 (setf (iop-end op) (+ (iop-start op) remained))
                 (push-op parser op)
                 (incf (ip-position parser) (length d))))
              ((< (iop-num op) (length d))
               (let ((remained (- (length d) (iop-num op))))
                 (_*->em-strong parser (iop-num op) (iop-start op))
                 (when (*-can-open parser)
                   (setf (iop-num op) remained)
                   (setf (iop-start op) (fill-pointer (ip-queue parser)))
                   (setf (iop-end op) (+ (iop-start op) remained))
                   (push-op parser op))
                 (push-string parser (repeat-char #\* remained))
                 (incf (ip-position parser) (length d)))))))))

(defun _*->em-strong (parser len start) ; consume *** => len = 3
  (multiple-value-bind (s-num e-num) (floor len 2)
    (loop :for i :from 0 :to (1- s-num)
          :do (replace-string parser "<strong>" (+ start (* 2 i)) (+ start (* 2 i) 2)))
    (when (= 1 e-num)
      (replace-string parser "<em>" (+ start (* s-num 2)) (+ start len))
      (push-string parser "</em>"))
    (loop :for i :from 0 :to (1- s-num)
          :do (push-string parser "</strong>"))))

;;
(defun scan-html-tag (parser)
  (if (scan&push parser *html-tag*)
      (run parser)))

;;
(defun scan-line-break (parser)
  (cond ((scan&+ parser '(:sequence :start-anchor (:greedy-repetition 2 nil #\Space) :end-anchor))
         (run parser))
        ((scan&+ parser '(:sequence :start-anchor (:greedy-repetition 2 nil #\Space) #\Newline))
         (push-string parser (format nil "<br />~%"))
         (run parser))
        ((scan&+ parser '(:sequence :start-anchor #\Space #\Newline))
         (push-string parser (format nil "~%"))
         (run parser))))

;;
(defun scan\-escape (parser)
  (cond ((scan parser `(:sequence :start-anchor #\\ (:char-class ,@*ascii-punctuations*)))
         (pos+ parser)
         (push-chars parser (read-c parser))
         (run parser))
        ((scan parser '(:sequence :start-anchor #\\ #\Newline))
         (pos+ parser 2)
         (push-string parser (format nil "<br />~%"))
         (run parser))))

;;
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

;;
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

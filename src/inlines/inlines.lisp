(defpackage :clcm/inlines/inlines
  (:use :cl
        :clcm/utils
        :clcm/raw-html-regex
        :clcm/inlines/inlines-ltgtamp
        :clcm/characters
        :clcm/inlines/parser)
  (:import-from :cl-ppcre)
  (:export :inlines->html))
(in-package :clcm/inlines/inlines)

;; api
(defun inlines->html (strings &key last-break)
  (if (null strings) (return-from inlines->html ""))
  (let ((chars (format nil "~{~A~^~%~}~A" strings (if last-break #\Newline ""))))
    (chars->html chars)))

;; main function
(defun chars->html (chars)
  (let ((parser (make-inlines-parser :input chars)))
    (run parser)
    (output parser)))

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

(defun output (parser)
  (format nil "~A" (ip-queue parser)))

;; emphasize
(defun scan-*_ (parser)
  (if (not (scan parser "^\\*")) (return-from scan-*_ nil))
  (when (or (close-* parser)
            (open-* parser))
    (run parser)))

(defun *-can-open (parser)
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
                 (incf (ip-position parser) (- (length d) remained)))))))))

(defun _*->em-strong (parser len start) ; consume *** => len = 3
  (multiple-value-bind (s-num e-num) (floor len 2)
    (when (= 1 e-num)
      (replace-string parser "<em>" start (+ start 1))
      (push-string parser "</em>"))
    (loop :for i :from 0 :to (1- s-num)
          :do (replace-string parser "<strong>"
                              (+ start (* e-num 4) (* 2 i))
                              (+ start (* e-num 4) (* 2 i) 2)))
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

;;
(defun is-backslash (char)
  (and char (char= char #\\)))

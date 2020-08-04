(defpackage :clcm/inlines/inlines
  (:use :cl
        :clcm/utils
        :clcm/raw-html-regex
        :clcm/characters
        :clcm/inlines/special-characters
        :clcm/inlines/parser)
  (:import-from :cl-ppcre)
  (:export :inlines->html
           :inlines->html*))
(in-package :clcm/inlines/inlines)

;; api
(defun inlines->html (strings &key last-break)
  (%inlines->html strings 
                  (lambda (parser)
                    (or (scan\-escape parser)
                        (scan-code-span parser)
                        (scan-html-tag parser)
                        (scan-line-break parser)
                        (scan-*_ parser)
                        (scan-special-characters parser)
                        (push-chars parser (read-c parser))))
                  last-break))

(defun inlines->html* (strings &key last-break)
  ;; only
  ;; < -> &lt;
  ;; > -> &gt;
  ;; & -> &amp;
  ;; double quote -> &quot;
  (%inlines->html strings 
                  (lambda (parser)
                    (or (scan-special-characters parser)
                        (push-chars parser (read-c parser))))
                  last-break))

;; main functions
(defun %inlines->html (strings proc last-break)
  (if (null strings) (return-from %inlines->html ""))
  (let* ((chars (format nil "~{~A~^~%~}~A" strings (if last-break #\Newline "")))
         (parser (make-inlines-parser :input chars)))
      (loop :while (peek-c parser)
            :do (funcall proc parser))
    (output parser)))

(defun output (parser)
  (format nil "~A" (ip-queue parser)))

;; emphasize
(defun scan-*_ (parser)
  (when (or (scan parser "^\\*") (scan parser "^_"))
    (or (close-*_ parser)
        (open-*_ parser))))

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

(defun _-can-open (parser)
  ;; TODO
  (or/mv (scan-to-strings parser
                          `(:sequence :modeless-start-anchor
                            (:regex "(_+)")
                            (:inverted-char-class  ,@`,(append *unicode-whitespaces* *punctuations*))))
         (if (/= (ip-position parser) 0)
             (scan-to-strings parser
                              `(:sequence :modeless-start-anchor
                                (:char-class
                                 ,@`,(append *unicode-whitespaces* *punctuations-without-**))
                                (:regex "(_+)")
                                (:char-class ,@*punctuations-without-**))
                              :offset -1)
             (scan-to-strings parser
                              `(:sequence :modeless-start-anchor
                                (:regex "(_+)")
                                (:char-class ,@*punctuations-without-**))))))

(defun open-*_ (parser)
  (multiple-value-bind (result s) (or/mv (*-can-open parser) (_-can-open parser))
    (when result
      (let* ((d (aref s 0)) ; d is delimiter run : ex. **
             (type (aref (aref s 0) 0))
             (start (fill-pointer (ip-queue parser)))
             (op (make-inline-op  :type type :start start :end (+ start (length d)) :num (length d))))
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

(defun _-can-close (parser)
  ;; TODO
  (if (= (ip-position parser) 0) (return-from _-can-close nil))
  (or/mv (scan-to-strings parser
                          `(:sequence :modeless-start-anchor
                            (:inverted-char-class ,@`,(append *unicode-whitespaces* *punctuations*))
                            (:regex "(_+)"))
                          :offset -1)
         (scan-to-strings parser
                          `(:sequence :modeless-start-anchor
                            (:char-class ,@*punctuations*)
                            (:regex "(_+)")
                            (:alternation
                             :modeless-end-anchor-no-newline
                             (:char-class
                              ,@`,(append *unicode-whitespaces* *punctuations-without-**))))
                          :offset -1)))

(defun close-*_ (parser)
  (multiple-value-bind (result s) (or/mv (*-can-close parser) (_-can-close parser))
    (when result
      (let* ((d (aref s 0)) ; d is delimiter run : ex. **
             (type (aref (aref s 0) 0))
             (op (pop-op parser type)))
        (cond ((null op) nil)
              ; NOTE incf do not return nil
              ((= (iop-num op) (length d))
               (*_->em-strong parser (length d) (iop-start op))
               (incf (ip-position parser) (length d)))
              ((> (iop-num op) (length d))
               (let ((remained (- (iop-num op) (length d))))
                 (*_->em-strong parser (length d) (+ (iop-start op) remained))
                 (setf (iop-num op) remained)
                 (setf (iop-end op) (+ (iop-start op) remained))
                 (push-op parser op)
                 (incf (ip-position parser) (length d))))
              ((< (iop-num op) (length d))
               (let ((remained (- (length d) (iop-num op))))
                 (*_->em-strong parser (iop-num op) (iop-start op))
                 (incf (ip-position parser) (- (length d) remained)))))))))

(defun *_->em-strong (parser len start) ; consume *** => len = 3
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
  (scan&push parser *html-tag*))

;;
(defun scan-line-break (parser)
  (or (scan&+ parser '(:sequence :start-anchor (:greedy-repetition 2 nil #\Space) :end-anchor))
      (and (scan&+ parser '(:sequence :start-anchor (:greedy-repetition 2 nil #\Space) #\Newline))
           (push-string parser (format nil "<br />~%")))
      (and (scan&+ parser '(:sequence :start-anchor #\Space #\Newline))
           (push-string parser (format nil "~%")))))

;;
(defun scan\-escape (parser)
  (or (and (scan parser "^\\\\\"")
           (pos+ parser 2)
           (push-string parser "&quot;"))
      (and (scan parser "^\\\\<")
           (pos+ parser 2)
           (push-string parser "&lt;"))
      (and (scan parser "^\\\\>")
           (pos+ parser 2)
           (push-string parser "&gt;"))
      (and (scan parser "^\\\\&")
           (pos+ parser 2)
           (push-string parser "&amp;"))
      (and (scan parser `(:sequence :start-anchor #\\ (:char-class ,@*ascii-punctuations*)))
           (pos+ parser)
           (push-chars parser (read-c parser)))
      (and (scan parser '(:sequence :start-anchor #\\ #\Newline))
           (pos+ parser 2)
           (push-string parser (format nil "<br />~%")))))

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
                 (push-string parser (format nil "<code>~A</code>" (inlines->html* (list content))))))
              (t
               (pos+ parser (length start-backticks))
               (push-string parser start-backticks))))))))

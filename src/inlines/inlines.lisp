(defpackage :clcm/inlines/inlines
  (:use :cl
        :clcm/utils
        :clcm/raw-html-regex
        :clcm/characters
        :clcm/inlines/parser
        :clcm/inlines/special-characters
        :clcm/inlines/emphasis)
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
                        (scan-emphasis parser)
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

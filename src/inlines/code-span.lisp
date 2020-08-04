(defpackage :clcm/inlines/code-span
  (:use :cl
        :clcm/inlines/parser)
  (:import-from :cl-ppcre)
  (:export :scan-code-span))
(in-package :clcm/inlines/code-span)

(defun scan-code-span (parser inlines->html*)
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
                 (push-string parser (format nil "<code>~A</code>" (funcall inlines->html* (list content))))))
              (t
               (pos+ parser (length start-backticks))
               (push-string parser start-backticks))))))))


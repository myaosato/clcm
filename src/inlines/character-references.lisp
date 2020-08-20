(defpackage :clcm/inlines/character-references
  (:use :cl
        :clcm/inlines/entities-data
        :clcm/inlines/parser)
  (:export :scan-character-references))
(in-package :clcm/inlines/character-references)

(defun scan-character-references (parser)
  (let ((result (scan-to-strings parser
                                 "^&(?:#[xX][a-fA-F0-9]{1,6}|#[0-9]{1,7}|[A-z][A-z0-9]{1,31});")))
    (when result
      (cond ((char= (char result 1) #\#) ; numeric character reference
             (let* ((char (code-char (if (or (char= (char result 2) #\x) (char= (char result 2) #\X))
                                         (parse-integer (subseq result 3 (1- (length result))) 
                                                        :radix 16)
                                         (parse-integer (subseq result 2 (1- (length result)))))))
                    (str (format nil "~A" (if (char= char #\Nul) (code-char #xFFFD) char))))
               (push-string parser (escape str))
               (pos+ parser (length result))))
            (t ; character entity reference
             (let ((str (gethash result *entities-map*)))
               (when str
                 (push-string parser (escape str))
                 (pos+ parser (length result)))))))))

(defun escape (str)
  (cond ((string= str "\"")
         "&quot;")
        ((string= str "<")
         "&lt;")
        ((string= str ">")
         "&gt;")
        ((string= str "&")
         "&amp;")
        (t
         str)))

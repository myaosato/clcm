(defpackage :clcm/inlines/character-references
  (:use :cl
        :clcm/inlines/entities-data
        :clcm/inlines/parser)
  (:export :scan-character-references))
(in-package :clcm/inlines/character-references)

(defun scan-character-references (parser)
  (let ((result (scan-to-strings parser "^&(?:#x[a-f0-9]{1,6}|#[0-9]{1,7}|[A-z][A-z0-9]{1,31});")))
    (when result
      (cond ((char= (char result 1) #\#) ; numeric character reference
             nil)
            (t ; character entity reference
             (let ((str (gethash result *entities-map*)))
               (when str
                 (cond ((string= str "\"")
                        (push-string parser "&quot;"))
                       ((string= str "<")
                        (push-string parser "&lt;"))
                       ((string= str ">")
                        (push-string parser "&gt;"))
                       ((string= str "&")
                        (push-string parser "&amp;"))
                       (t
                        (push-string parser str)))
                 (pos+ parser (length result)))))))))

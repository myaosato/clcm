(defpackage :clcm/raw-html
  (:use :cl)
  (:import-from :cl-ppcre
                :scan-to-strings)
  (:export :check-raw-html))
(in-package :clcm/raw-html)

(defparameter re-tagname "[A-Za-z][-0-9A-Za-z]*")
(defparameter re-spaces? "[ \\t]*[\\n\\r]?[ \\t]*") ;; optional spaces, tabs, and up to one line ending
(defparameter re-spaces+ (format nil "(?:[ \\t\\n\\r]|~A)" re-spaces?)) ;; spaces, tabs, and up to one line ending
(defparameter re-attribute-name "[A-Za-z_:][-_.:0-9A-Za-z]*")
(defparameter re-unquoted-value "[^ \\t\\n\\r\"'=<>`]+")
(defparameter re-single-quoted-value "'[^']*'")
(defparameter re-double-quoted-value "\"[^\"]*\"")
(defparameter re-attribute-value (format nil "(?:~A|~A|~A)"
                                         re-unquoted-value
                                         re-single-quoted-value
                                         re-double-quoted-value))
(defparameter re-attribute-value-spec (format nil "~A=~A~A" re-spaces? re-spaces? re-attribute-value))
(defparameter re-attribute (format nil "~A~A(?:~A)?" re-spaces+ re-attribute-name re-attribute-value-spec))
(defparameter re-open-tag (format nil "<~A(?:~A)*~A/?>" re-tagname re-attribute re-spaces?))
(defparameter re-closing-tag (format nil "</~A~A>" re-tagname re-spaces?))
(defparameter re-comment "<!---->|<!--(?:-?[^>-])(?:-?[^-])*-->")
(defparameter re-processing-instruction "<\\?[\\s\\S]*?\\?>")
(defparameter re-declaration "<![A-Za-z][^>]*>")
(defparameter re-cdata-section "<!\\[CDATA\\[[\\s\\S]*?\\]\\]>")
(defparameter re-html-tag (format nil "^(?:~A|~A|~A|~A|~A|~A)"
                                  re-open-tag
                                  re-closing-tag
                                  re-comment
                                  re-processing-instruction
                                  re-declaration
                                  re-cdata-section))
(defun check-raw-html (lines pos)
  (let* ((raw-html (scan-to-strings re-html-tag lines :start pos)))
    (when raw-html
      (cons (list :raw-html raw-html) (+ pos (length raw-html))))))

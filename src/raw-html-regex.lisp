(defpackage :clcm/raw-html-regex
  (:use :cl)
  (:export :*open-tag*
           :*closing-tag*
           :*html-tag*))
(in-package :clcm/raw-html-regex)

;; ref. https://spec.commonmark.org/0.29/#raw-html

(defvar *tag-name*
  '(:regex "[a-zA-Z][a-zA-Z-\\d]*"))

(defvar *attribute-name*
  '(:regex "[a-zA-Z_:][a-zA-Z_:.-\\d]*"))

(defvar *unquoted-attribute-value*
  '(:regex "[^\\s\"'=<>`]+"))

(defvar *single-quoted-attribute-value*
  '(:regex "'[^']*'"))

(defvar *double-quoted-attribute-value*
  '(:regex "\"[^\"]*\""))

(defvar *attribute-value*
  `(:alternation
    ,*unquoted-attribute-value*
    ,*single-quoted-attribute-value*
    ,*double-quoted-attribute-value*))

(defvar *attribute-value-specification*
  `(:sequence
    (:greedy-repetition 0 nil :whitespace-char-class)
    "="
    (:greedy-repetition 0 nil :whitespace-char-class)
    ,*attribute-value*))

(defvar *attribute*
  `(:sequence
    (:greedy-repetition 1 nil :whitespace-char-class)
    ,*attribute-name*
    (:greedy-repetition 0 1 ,*attribute-value-specification*)))


(defvar *open-tag*
  `(:sequence
    "<"
    ,*tag-name*
    (:greedy-repetition 0 nil ,*attribute*)
    (:greedy-repetition 0 nil :whitespace-char-class)
    (:greedy-repetition 0 1 "/")
    ">"))

(defvar *closing-tag*
  `(:sequence
    "</"
    ,*tag-name*
    (:greedy-repetition 0 nil :whitespace-char-class)
    ">"))

(defvar *html-comment*
  `(:sequence
    "<!--"
    (:alternation
     (:regex "")
     (:sequence 
      (:alternation
       (:regex "[^->]")
       (:regex "-[^->]"))
      (:non-greedy-repetition 0 nil (:regex "[^-]*?|-[^-]"))))
    "-->"))

(defvar *processiong-instruction*
  `(:sequence
    "<?"
    (:non-greedy-repetition 0 nil :everything)
    "?>"))

(defvar *declaration*
  '(:regex "<![A-Z]+ [^>]*?>"))

(defvar *cdata-section*
  `(:sequence
    "<![CDATA["
    (:non-greedy-repetition 0 nil :everything)
    "]]>"))

(defvar *html-tag*
  `(:sequence
    :start-anchor
    (:alternation
     ,*open-tag*
     ,*closing-tag*
     ,*html-comment*
     ,*processiong-instruction*
     ,*declaration*
     ,*cdata-section*)))

(defpackage :clcm/characters
  (:use :cl)
  (:export :ascii-punctuations-without-*_*
           :*unicode-whitespaces*
           :*ascii-punctuations*
           :*punctuations*
           :*punctuations-without-**
           :*punctuations-without-_*))
(in-package :clcm/characters)

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


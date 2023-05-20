(defpackage :clcm/emphasis
  (:import-from :cl-ppcre
                :scan
                :scan-to-strings)
  (:import-from :cl-unicode
                :general-category)
  (:export :check-emphasis)
  (:use :cl))
(in-package :clcm/emphasis)

(defun char? (string index)
  (when (< -1 index (length string))
    (char string index)))

(defun char=? (character &rest more-characters)
  (cond ((null more-characters)
         t)
        ((eql character (car more-characters))
         (apply #'char=? character (cdr more-characters)))
        (t
         nil)))

(defun char/=? (character &rest more-characters)
  (not (apply #'char=? character more-characters)))

(defun unicode-whitespace-character-p (char)
  (when char
    (or (find (char-code char) '(#x9 #xA #xC #xD) :test #'=)
        (string= (general-category char) "Zs"))))

(defparameter ascii-punctuation-character "[-!\"#$%&'()*+,.:;<=>?@[\\]^_`{|}~\\\\]")
(defun ascii-punctuation-character-p (char)
  (when char
    (scan ascii-punctuation-character (format nil "~A" char))))

(defun unicode-punctuation-character-p (char)
  (when char
    (or (ascii-punctuation-character-p char)
        (find (general-category char) '("Pc" "Pd" "Pe" "Pf" "Pi" "Po" "Ps") :test #'string=))))

(defun pick-delimiter-run (pos lines)
  (let ((candidate (or (scan-to-strings "^\\*+(?!\\*)" lines :start pos)
                       (scan-to-strings "^_+(?!_)" lines :start pos))))
    (unless candidate
      (return-from pick-delimiter-run))
    (when (or (and (char=? #\* (char candidate 0))
                   (or (char/=? #\* (char? lines (- pos 1)))
                       (char=? #\\ (char? lines (- pos 2)))))
              (and (char=? #\_ (char? candidate 0))
                   (or  (char/=? #\_ (char? lines (- pos 1)))
                        (char=? #\\ (char? lines (- pos 2))))))
      candidate)))

(defun followed-by-unicode-whitespace-p (delimiter-run pos lines)
  (unicode-whitespace-character-p (char? lines (+ (length delimiter-run) pos))))

(defun followed-by-unicode-punctuation-p (delimiter-run pos lines)
  (unicode-punctuation-character-p (char? lines (+ (length delimiter-run) pos))))

(defun preceded-by-unicode-whitespace-p (pos lines)
  (unicode-whitespace-character-p (char? lines (1- pos))))

(defun preceded-by-unicode-punctuation-p (pos lines)
  (unicode-punctuation-character-p (char? lines (1- pos))))

(defun left-flanking-delimiter-run-p (delimiter-run pos lines)
  (and (not (followed-by-unicode-whitespace-p delimiter-run pos lines))
       (or (not (followed-by-unicode-punctuation-p delimiter-run pos lines))
           (preceded-by-unicode-whitespace-p pos lines)
           (preceded-by-unicode-punctuation-p pos lines))))

(defun right-flanking-delimiter-run-p (delimiter-run pos lines)
  (and (not (preceded-by-unicode-whitespace-p pos lines))
       (or (not (preceded-by-unicode-punctuation-p pos lines))
           (followed-by-unicode-whitespace-p delimiter-run pos lines)
           (followed-by-unicode-punctuation-p delimiter-run pos lines))))


(defun check-emphasis (lines pos)
  (let ((delimiter-run (pick-delimiter-run pos lines)))
    (unless delimiter-run
      (return-from check-emphasis nil))
    (let ((is-left-flanking (left-flanking-delimiter-run-p delimiter-run pos lines))
          (is-right-flanking (right-flanking-delimiter-run-p delimiter-run pos lines)))
      (cond ((char= #\* (char delimiter-run 0))
             (let ((is-opener is-left-flanking)
                   (is-closer is-right-flanking))
               `(:emphasis (:emphasis-* ,delimiter-run ,is-opener ,is-closer) ,(+ pos (length delimiter-run)))))
            ((char= #\_ (char delimiter-run 0))
             (let ((is-opener (and is-left-flanking 
                                   (or (not is-right-flanking)
                                       (preceded-by-unicode-punctuation-p pos lines))))
                   (is-closer (and is-right-flanking 
                                   (or (not is-right-flanking)
                                       (followed-by-unicode-punctuation-p delimiter-run pos lines)))))
               `(:emphasis (:emphasis-_ ,delimiter-run ,is-opener ,is-closer) ,(+ pos (length delimiter-run)))))))))

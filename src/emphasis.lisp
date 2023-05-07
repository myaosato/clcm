(defpackage :clcm/emphasis
  (:import-from :cl-ppcre
                :scan
                :scan-to-strings)
  (:import-from :cl-unicode
                :general-category)
  (:use :cl))
(in-package :clcm/emphasis)

(defun unicode-whitespace-character-p (char)
  (or (find (char-code char) '(#x9 #xA #xC #xD) :test #'=)
      (string= (general-category char) "Zs")))

(defparameter ascii-punctuation-character "[-!\"#$%&'()*+,.:;<=>?@[\\]^_`{|}~\\\\]")
(defun ascii-punctuation-character-p (char)
  (scan ascii-punctuation-character (format nil "~A" char)))

(defun unicode-punctuation-character-p (char)
  (or (ascii-punctuation-character-p char)
      (find (general-category char) '("Pc" "Pd" "Pe" "Pf" "Pi" "Po" "Ps") :test #'string=)))

(defun pick-delimiter-run (pos lines)
  (let ((candidate (or (scan-to-strings "^\\*+(?!\\*)" lines :start pos)
                       (scan-to-strings "^_+(?!_)" lines :start pos))))
    (when (or (and (char= #\* (char candidate 0))
                   (or (char/= #\* (char lines (- pos 1)))
                       (char= #\\ (char lines (- pos 2)))))
              (and (char= #\_ (char candidate 0))
                   (or (char/= #\_ (char lines (- pos 1)))
                       (char= #\\ (char lines (- pos 2))))))
      candidate)))

(defun followed-by-unicode-whitespace-p (delimiter-run pos lines)
  (unicode-whitespace-character-p (char lines (+ (length delimiter-run) pos))))

(defun followed-by-unicode-punctuation-p (delimiter-run pos lines)
  (unicode-pubctuation-character-p (char lines (+ (length delimiter-run) pos))))

(defun preceded-by-unicode-whitespace-p (pos lines)
  (unicode-whitespace-character-p (char lines (1- pos))))

(defun preceded-by-unicode-punctuation-p (pos lines)
  (unicode-pubctuation-character-p (char lines (1- pos))))

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


(defun check-emphasis (pos lines)
  (let ((delimiter-run (pick-delimiter-run pos lines)))
    (unless delimiter-run
      (return-from check-emphasis nil))
    (let ((is-left-flanking (left-flanking-delimiter-run delimiter-run pos lines))
          (is-right-flanking (right-flanking-delimiter-run delimiter-run pos lines)))
      ;; TODO
      )))

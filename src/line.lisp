(defpackage :clcm/line
  (:use :cl)
  (:import-from :cl-ppcre
                :scan
                :scan-to-strings)
  (:export :string->lines
           :is-blank-line
           :skip-blank-line?
           :get-indented-depth-of
           :*white-space-characters*))
(in-package :clcm/line)

;; utils
(defvar *white-space-characters* (mapcar #'code-char '(#x20 #x09 #x0A #x0B #x0C #x0D)))

(defun string->lines (input)
  (lines input 0 nil))

(defun lines (input pos output)
  (if (>= pos (length input))
      (reverse output)
      (line input pos output nil)))

(defun line (input pos output buf)
  (labels ((buf->line (chars) (concatenate 'string (reverse chars))))
    (if (>= pos (length input))
        (return-from line  (lines input pos (if buf (cons (buf->line buf) output)))))
    (let ((char (char input pos)))
      (cond ((char= char (code-char 10))
             (lines input (1+ pos) (cons (buf->line buf) output)))
            ((char= char (code-char 13))
             (if (char= (char input (1+ pos)) (code-char 10))
                 (lines input (+ 2 pos) (cons (buf->line buf) output))
                 (lines input (1+ pos) (cons (buf->line buf) output))))
            (t (line input (1+ pos) output (cons char buf)))))))

;; blank line
(defun is-blank-line (line)
  (scan `(:sequence 
          :start-anchor
          (:greedy-repetition 0 nil (:alternation ,(code-char #x20) ,(code-char #x09)))
          :end-anchor)
        line))

(defun skip-blank-line? (line)
  (is-blank-line line))


;; indent
(defun get-indented-depth-of (line offset)
  (if (= 0 (length line)) (return-from get-indented-depth-of 0))
  (let ((depth 0))
    (loop :named search
          :for c :across line
          :do (cond ((char= c #\Space)
                     (incf depth))
                    ((char= c #\Tab)
                     (incf depth (- 4 (rem (+ offset depth) 4))))
                    (t (return-from search depth)))
          :if (> depth 3)
          :return depth)))

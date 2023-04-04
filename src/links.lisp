(defpackage :clcm/links
  (:use :cl)
  (:import-from :cl-ppcre
                :scan-to-strings)
  (:export :check-link-opener
           :check-link-closer))
(in-package :clcm/links)

(defparameter link-open "^\\[")
(defparameter link-title (format nil "^(?:~A|~A|~A)"
                                 "\"(?:\\\\\"|[^\"])*\""
                                 "'(?:\\\\'|[^'])*'"
                                 "\\((?:\\\\\\(|\\\\\\)|[^()])*\\)"))

;; check-link-opener
(defun check-link-opener (lines pos)
  (when (scan-to-strings link-open lines :start pos)
    (cons (list :link-opener "[") (1+ pos))))

;; check-link-closer
(defun check-close-link-text (lines pos)
  (scan-to-strings "^]\\(" lines :start pos))

(defun check-destination (lines pos)
  (let ((<dest> (scan-to-strings "^<(?:[^<>\\n\\r]|\\\\<|\\\\>)*>" lines :start pos)))
    (when <dest>
      (return-from check-destination
        (cons (subseq <dest> 1 (1- (length <dest>))) (+ pos (length <dest>)))))
    (when (char= (char lines pos) #\<)
      (return-from check-destination nil))
    (loop :with result := nil
          :with p := pos
          :with open-parentheses-counter := 0
          :while (< p (length lines))
          :for c := (char lines p)
          :do (cond ((and (char= c #\\)
                          (< (1+ p) (length lines))
                          (scan-to-strings "^[-!\"#$%&'()*+,.:;<=>?@[\\]^_`{|}~\\\\]" lines :start p))
                     (incf p))
                    ((scan-to-strings "^[ \\x00-\\x1F\\x7F]" lines :start p)
                     (return-from check-destination
                       (cons (coerce (reverse result) 'string) p)))
                    ((char= c #\()
                     (incf open-parentheses-counter))
                    ((char= c #\))
                     (if (> open-parentheses-counter 0)
                         (decf open-parentheses-counter)
                         (return-from check-destination
                           (cons (coerce (reverse result) 'string) p)))))
          :do (push c result)
          :do (incf p))))

(defun check-title (lines pos)
  (let ((title (scan-to-strings link-title lines :start pos)))
    (when title
      (cons (subseq title 1 (1- (length title))) (+ pos (length title))))))

(defun check-close-link (lines pos)
  (scan-to-strings "^\\)" lines :start pos))

(defun check-separater (lines pos)
  (+ pos (length (scan-to-strings "^[ \\t]*[\\n\\r]?[ \\t]*" lines :start pos))))

(defun check-link-closer (lines pos)
  (let ((p pos)
        (dest nil)
        (title nil))
    (unless (check-close-link-text lines p)
      (return-from check-link-closer))
    (incf p 2)
    (setf p (check-separater lines p))
    (let ((result (check-destination lines p)))
      (when result
        (destructuring-bind (text . next) result
          (setf p next)
          (setf dest text))))
    (setf p (check-separater lines p))
    (let ((result (check-title lines p)))
      (when result
        (destructuring-bind (text . next) result
          (setf p next)
          (setf title text))))
    (setf p (check-separater lines p))
    (unless (check-close-link lines p)
      (return-from check-link-closer))
    (cons (list :link-closer dest title) (1+ p))))

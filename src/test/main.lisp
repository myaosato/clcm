(defpackage :clcm/test/main
  (:use :cl)
  (:nicknames :clcm/test)
  (:shadow)
  (:import-from :clcm/clcm :cm->html)
  (:import-from :asdf :system-source-directory)
  (:import-from :cl-json :decode-json-from-source)
  (:export :test
           :test-for
           :test-range))
(in-package :clcm/test/main)

(defvar *spec-json-file* (merge-pathnames "spec.json"
                                          (system-source-directory "clcm")))

(defun test ()
  (let* ((test-data (decode-json-from-source *spec-json-file*))
         (sections nil)
         (number-of-case (length test-data)))
    (loop :for test-case :in test-data
          :for ind :from 0 :to (1- number-of-case)
          ; separate for each sections
          :unless (equal (caar sections) (cdr (assoc :section test-case)))
          :do (setf sections (cons (list (cdr (assoc :section test-case)) 0 0) sections))
          :end
          ; test     
          :if (string= (cdr (assoc :html test-case))
                       (cm->html (cdr (assoc :markdown test-case))))
          :do (incf (second (car sections)))
          :end
          :do (incf (third (car sections))))
    ;; TODO inform more detail
    (loop :for result :in (reverse sections)
          :do (format t "~{~A:~42T~A~46T/ ~A~%~}" result))
    (let ((passed (reduce (lambda (acc elt) (+ acc (cadr elt))) sections :initial-value 0))
          (total number-of-case))
      (format t 
              "~%TOTAL:~42T~A~46T/ ~A test cases (~,2F%)~%"
              passed
              total
              (/ passed total)))))

(defun test-for (num)
  (let* ((test-data (decode-json-from-source *spec-json-file*))
         (test-case (nth (1- num) test-data))
         (result (cm->html (cdr (assoc :markdown test-case)))))
    (if (string= (cdr (assoc :html test-case)) result)
        (values t (format nil "OK~%"))
        (values nil (format nil "NG~%~A~%" result)))))

(defun test-range (start end)
  (loop :for ind :from start :to end
        :do (format t "~A: ~A~%" ind (clcm/test:test-for ind))))

(defun test-section (key)
  (case key
    (:tabs (test-range 1 11))
    (:precedence (test-range 12 12))
    (:thematic-breaks (test-range 13 31))
    (:atx-headings (test-range 32 49))
    (:setext-headings (test-range 50 76))
    (:indented-code-blocks (test-range 77 88))
    (:fenced-code-blocks (test-range 89 117))
    (:html-blocks (test-range 118 160))
    (:link-reference-definitions (test-range 161 188))
    (:paragraphs (test-range 189 196))
    (:blank-lines (test-range 197 197))
    (:block-quotes (test-range 198 222))
    (:list-items (test-range 223 270))
    (:lists (test-range 271 296))))

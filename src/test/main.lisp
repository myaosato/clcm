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
    (format t 
            "~%TOTAL:~42T~A~46T/ ~A test cases~%"
            (reduce (lambda (acc elt) (+ acc (cadr elt))) sections :initial-value 0)
            number-of-case)))

(defun test-for (num)
  (let* ((test-data (decode-json-from-source *spec-json-file*))
         (test-case (nth (1- num) test-data)))
    (if (string= (cdr (assoc :html test-case))
                 (cm->html (cdr (assoc :markdown test-case))))
        (values t (format nil "OK~%"))
        (values nil (format nil "NG~%~A~%" (cm->html (cdr (assoc :markdown test-case))))))))

(defun test-range (start end)
  (loop :for ind :from start :to end
        :do (multiple-value-bind (res disp) (clcm/test:test-for ind)
              (unless res
                (format t "~A:~%~A~%~%" ind disp)))))

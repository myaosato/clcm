(defpackage :clcm/test/main
  (:use :cl) 
  (:shadow)
  (:import-from :clcm/clcm :cm->html)
  (:import-from :asdf :system-source-directory)
  (:import-from :cl-json :decode-json-from-source)
  (:export :test))
(in-package :clcm/test/main)

(defvar *spec-json-file* (merge-pathnames "spec.json"
                                          (system-source-directory "clcm")))

(defun test ()
  (let ((test-data (decode-json-from-source *spec-json-file*))
        (ok-cases nil)
        (ng-cases nil)
        (err-cases nil))
    (loop :for test-case :in test-data
          :for ind :from 0 :to (1- (length test-data))
          :for result := (progn
                           (if (string= (cdr (assoc :html test-case))
                                        (cm->html (cdr (assoc :markdown test-case))))
                               (push test-case ok-cases)
                               (push test-case ng-cases)))
          :unless result
          :do (push test-case err-cases))
    ;; TODO inform more detail
    (format t "OK: ~A, NG: ~A, ERR: ~A" (length ok-cases) (length ng-cases) (length err-cases))))

(defun test-for (num)
  (let* ((test-data (decode-json-from-source *spec-json-file*))
         (test-case (nth (1- num) test-data)))
    (if (string= (cdr (assoc :html test-case))
                 (cm->html (cdr (assoc :markdown test-case))))
        (format t "OK~%")
        (format t "NG~%~A~%" (cm->html (cdr (assoc :markdown test-case)))))))
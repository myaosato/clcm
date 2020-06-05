(defpackage :clcm/clcm
  (:nicknames :clcm)
  (:use :cl
        :clcm/utils)
  (:import-from :clcm/tree
                :make-tree
                :tree->html)
  (:export :cm->html))

(in-package :clcm/clcm)

;; main function
(defun cm->html (cm-string)
  ;; convert from CommonMark string to HTML string
  (-> cm-string
      (make-tree)
      (tree->html)))

(defpackage :clcm/inlines
  (:use :cl)
  (:import-from :uiop
                :if-let)
  (:import-from :clcm/code-spans
                :check-code-spans)
  (:import-from :clcm/autolinks
                :check-autolinks)
  (:import-from :clcm/line-breaks
                :check-line-breaks)
  (:import-from :clcm/raw-html
                :check-raw-html)
  (:import-from :clcm/backslash-escapes
                :check-backslash-escapes)
  (:import-from :clcm/links
                :check-link-opener
                :check-image-opener
                :check-link-closer
                :check-close-link-text)
  (:import-from :clcm/parser/inlines
                :make-inline-parser
                :push-parsed
                :push-delimiter
                :search-delimiter
                :remove-delimiter
                :update-pointer
                :inactivate-before
                :parser-lines
                :parser-inlines
                :delimiter-is-active
                :delimiter-type
                :delimiter-content)
  (:export :parse-inline))
(in-package :clcm/inlines)

;; return next position (on lines)
(defun process-result (parser pos type parsed next)
  (when (eq type :link-opener)
    (push-delimiter parser parsed :link)
    (return-from process-result next))
  (when (eq type :image-opener)
    (push-delimiter parser parsed :image)
    (return-from process-result next))
  (when (eq type :link-closer)
    (let ((opener (search-delimiter parser :link :image)))
      (cond ((and opener (delimiter-is-active opener))
             (let ((closer (check-link-closer (parser-lines parser) (1+ pos))))
               (if closer
                   (destructuring-bind (_ (dest title) next-pos) closer
                     (declare (ignore _))
                     (let ((type (delimiter-type opener))
                           (content (delimiter-content opener)))
                       (update-pointer parser opener)
                       (push-parsed parser (list type content dest title))
                       (remove-delimiter parser opener)
                       (when (eq type :link)
                         (inactivate-before opener :link))
                       (setf next next-pos)))
                   (push-parsed parser parsed))))
            (opener ;; (not (delimiter-is-active opener)) case
                  (remove-delimiter parser opener)
                  (push-parsed parser parsed))
            (t
             (push-parsed parser parsed))))
    (return-from process-result next))
  (push-parsed parser parsed)
  next)

(defun parse-inline (lines)
  (let* ((parser (make-inline-parser lines)))
    (loop :with done := 0
          :with pos := 0
          :with lines := (parser-lines parser)
          :while (< pos (length lines))
          :for result := (or (check-backslash-escapes lines pos)
                             (check-code-spans lines pos)
                             (check-autolinks lines pos)
                             (check-line-breaks lines pos)
                             (check-raw-html lines pos)
                             (check-link-opener lines pos)
                             (check-image-opener lines pos)
                             (check-close-link-text lines pos))
          :if result
          :do (destructuring-bind (type parsed next) result
                (when (/= pos done)
                  (push-parsed parser (subseq lines done pos)))
                (let ((next (process-result parser pos type parsed next)))
                  (setf done next)
                  (setf pos next)))
          :else
          :do (incf pos)
          :finally (when (/= pos done)
                     (push-parsed parser (subseq lines done pos))))
    (cdr (parser-inlines parser))))

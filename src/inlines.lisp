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
  (:import-from :clcm/emphasis
                :check-emphasis)
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
                :parser-delimiters-bottom
                :delimiter-is-active
                :delimiter-type
                :delimiter-content
                :delimiter-next
                :delimiter-prev
                :delimiter-closer-p
                :delimiter-opener-p)
  (:export :parse-inline))
(in-package :clcm/inlines)

;; return next position (on lines)
(defun process-result (parser pos type parsed next)
  (cond ((eq type :emphasis)
         (destructuring-bind (delimiter-type delimiter-run is-opener is-closer) parsed
           (push-delimiter parser delimiter-run delimiter-type
                           :number-of (length delimiter-run)
                           :is-opener is-opener
                           :is-closer is-closer)))
        ((eq type :link-opener)
         (push-delimiter parser parsed :link))
        ((eq type :image-opener)
         (push-delimiter parser parsed :image))
        ((eq type :link-closer)
         (let ((opener (search-delimiter parser :link :image)))
           (cond ((null opener)
                  (push-parsed parser parsed))
                 ((not (delimiter-is-active opener))
                  (push-parsed parser parsed)
                  (remove-delimiter parser opener))
                 (t
                  (if (closer (check-link-closer (parser-lines parser) (1+ pos)))
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
                      (push-parsed parser parsed))))))
        (t 
         (push-parsed parser parsed)))
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


;; TODO 積まれているものの順番とか呼ばれ方とかあまりまともに考えられていないので要確認
(defun find-closer (current-position)
  (unless current-position
    (return-from find-closer))
  (if (and (delimiter-closer-p current-positon)
           (or (eq (delmiter-type current-positon) :emphasis-*)
               (eq (delmiter-type current-positon) :emphasis-_)))
      current-position
      (find-closer (delimiter-next current-position))))
(defun find-opener (delmiter openers-bottom type)
  (when (eq delimiter openers-bottom)
    (return-from find-opener))
  (if (and (delimiter-opener-p delimiter)
           (eq (delmiter-type delimiter) type))
      delimiter
      (find-opener (delimiter-prev delmiter) openers-bottom type)))
(defun parse-emphasis (parser &optional (bottom nil)) ;; TODO design return value
  (unless (parser-delimiters-bottom parser)
    (return-from parse-emphasis))
  (let ((current-position (if bottom
                              (delimiter-next bottom)
                              (parser-delimiters-bottom parser))))
        (openers-bottom bottom))
    (loop :with opener
          :while current-position
          :do (setf current-position (find-closer current-position))
          :do (setf opener (find-opener (delimiter-prev current-position) openers-bottom type))
          :do (cond (opener ;; found opener
                     ;; TODO
                     )
                    (t ;; not found opener
                     (setf openers-bottom (prev current-position))
                     (unless (delimiter-opener-p current-position)
                       (remove-delimiter parser current-position))
                     (setf current-potision (delmiter-next current-position))))
          :finally (remove-all-delimiter-above parser delimiter))))


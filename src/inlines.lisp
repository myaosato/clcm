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
                :remove-delimiters-between
                :remove-all-delimiters-above
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
                :delimiter-opener-p
                :delimiter-length
                :delimiter-pointer)
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
                  (let ((closer (check-link-closer (parser-lines parser) (1+ pos))))
                    (if closer
                        (destructuring-bind (_ (dest title) next-pos) closer
                          (declare (ignore _))
                          (let ((type (delimiter-type opener))
                                (content (delimiter-content opener)))
                            (update-pointer parser opener)
                            (push-parsed parser (list type content dest title))
                            (parse-emphasis parser opener)
                            (remove-delimiter parser opener)
                            (when (eq type :link)
                              (inactivate-before opener :link))
                            (setf next next-pos)))
                        (push-parsed parser parsed)))))))
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
    (parse-emphasis parser)
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
(defun reduce-delimiter (parser opener closer length)
  ;; return next closer
  (decf (delimiter-length opener) length)
  (decf (delimiter-length closer) length)
  (cond ((< 0 (delimiter-length opener))
         (setf (cadr (delimiter-pointer opener))
               (subseq (delimiter-pointer opener) 0 (- (length (delimiter-pointer opener) length)))))
        (t
         (setf (cdr (delimiter-pointer opener)) (cddr (delimiter-pointer opener)))
         (remove-delimiter parser opener)))
  (cond ((< 0 (delimiter-length closer))
         (setf (cadr (delimiter-pointer closer))
               (subseq (delimiter-pointer closer) 0 (- (length (delimiter-pointer closer) length))))
         closer)
        (t
         (setf (cdr (delimiter-pointer closer)) (cddr (delimiter-pointer closer)))
         (remove-delimiter parser closer)
         (delimiter-next closer))))
(defun parse-emphasis (parser &optional (bottom nil)) ;; TODO design return value
  (unless (parser-delimiters-bottom parser)
    (return-from parse-emphasis))
  (let ((closer (if bottom
                    (delimiter-next bottom)
                    (parser-delimiters-bottom parser))))
        (openers-bottom bottom))
    (loop :with opener
          :while closer
          :do (setf closer (find-closer closer))
          :do (setf opener (find-opener (delimiter-prev closer) openers-bottom type))
          :do (cond (opener ;; found opener
                     (let ((is-strong (and (<= 2 (delimiter-length closer))
                                           (<= 2 (delimiter-length opener))))
                           (new-element-pointer (cons nil nil))
                           (next-element-pointer (cdr (delimiter-pointer closer))))
                       (setf (cdr (delimiter-pointer closer)) nil)
                       (setf (car new-element)
                             (list (if is-strong :strong-emphasis :emphasis)
                                   (cddr (delimiter-pointer opener))))
                       (setf (cddr (delimiter-pointer opener)) new-element-pointer)
                       (setf (cdr new-element-pointer) next-element-pointer)
                       (setf (delimiter-pointer closer) new-element-pointer)
                       (remove-delimiters-between parser opener closer)
                       (setf closer (reduce-delimiter parser operner closer (if is-strong 2 1)))))
                    (t ;; not found opener
                     (setf openers-bottom (prev closer))
                     (unless (delimiter-opener-p closer)
                       (remove-delimiter parser closer))
                     (setf closer (delmiter-next closer))))
          :finally (remove-all-delimiter-above parser delimiter)))


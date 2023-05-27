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
                :delimiter-pointer
                :delimiter-pointer-prev)
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
                            (update-pointer parser (delimiter-pointer-prev opener))
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
                             (check-close-link-text lines pos)
                             (check-emphasis lines pos))
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
  (if (and (delimiter-closer-p current-position)
           (or (eq (delimiter-type current-position) :emphasis-*)
               (eq (delimiter-type current-position) :emphasis-_)))
      current-position
      (find-closer (delimiter-next current-position))))
(defun find-opener (delimiter openers-bottom type)
  (when (eq delimiter openers-bottom)
    (return-from find-opener))
  (if (and (delimiter-opener-p delimiter)
           (eq (delimiter-type delimiter) type))
      delimiter
      (find-opener (delimiter-prev delimiter) openers-bottom type)))
(defun reduce-delimiter (parser opener closer length)
  ;; return next closer
  (decf (delimiter-length opener) length)
  (decf (delimiter-length closer) length)
  (cond ((< 0 (delimiter-length opener))
         (setf (car (delimiter-pointer opener))
               (subseq (car (delimiter-pointer opener))
                       0
                       (- (length (car (delimiter-pointer opener))) length))))
        (t
         (setf (cdr (delimiter-pointer-prev opener)) (cdr (delimiter-pointer opener)))
         (remove-delimiter parser opener)))
  (cond ((< 0 (delimiter-length closer))
         (setf (car (delimiter-pointer closer))
               (subseq (car (delimiter-pointer closer))
                       0
                       (- (length (car (delimiter-pointer closer))) length)))
         closer)
        (t
         (setf (cdr (delimiter-pointer-prev closer)) (cdr (delimiter-pointer closer)))
         (remove-delimiter parser closer)
         (delimiter-next closer))))
(defun parse-emphasis (parser &optional (bottom nil)) ;; TODO design return value
  (unless (parser-delimiters-bottom parser)
    (return-from parse-emphasis))
  (let ((closer (find-closer (if bottom
                                 (delimiter-next bottom)
                                 (parser-delimiters-bottom parser))))
        (openers-bottom bottom))
    (loop :with opener
          :while closer
          :do (setf opener (find-opener (delimiter-prev closer) openers-bottom (delimiter-type closer)))
          :do (cond (opener ;; found opener
                     (let ((is-strong (and (<= 2 (delimiter-length closer))
                                           (<= 2 (delimiter-length opener))))
                           (new-element-pointer (cons nil nil)))
                       (setf (cdr (delimiter-pointer-prev closer)) nil)
                       (setf (car new-element-pointer)
                             (list (if is-strong :strong-emphasis :emphasis)
                                   (cdr (delimiter-pointer opener))))
                       (setf (cdr new-element-pointer) (delimiter-pointer closer))
                       (setf (delimiter-pointer-prev closer) new-element-pointer)
                       (setf (cdr (delimiter-pointer opener)) new-element-pointer)
                       (remove-delimiters-between parser opener closer)
                       (setf closer (reduce-delimiter parser opener closer (if is-strong 2 1)))))
                    (t ;; not found opener
                     (setf openers-bottom (delimiter-prev closer))
                     (unless (delimiter-opener-p closer)
                       (remove-delimiter parser closer))
                     (setf closer (delimiter-next closer))))
          :do (setf closer (find-closer closer))
          :finally (remove-all-delimiters-above parser bottom))))


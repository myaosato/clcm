(defpackage :clcm/inlines/link-image
  (:use :cl
        :clcm/utils
        :clcm/inlines/parser
        :clcm/inlines/emphasis)
  (:export :scan-open-link
           :scan-open-image
           :scan-close-link-image))
(in-package :clcm/inlines/link-image)

;; API
(defun scan-open-link (parser)
  (when (scan parser "^\\[")
    (push-op parser (make-delimiter :type :|[| 
                                    :start (fill-pointer (ip-queue parser))
                                    :num 1
                                    :active t))
    (push-string parser "[")
    (pos+ parser 1)))

(defun scan-open-image (parser)
  ; TODO
  parser
  nil)

(defun scan-close-link-image (parser)
  (when (scan parser "^]")
    (let ((open-pos (find-opening-delimiter parser)))
      (when open-pos
        (cond ((is-link-close parser)
               (process-emphasis parser (1+ open-pos))
               (make-link parser open-pos)
               (pop-stack-to-bottom parser open-pos)
               (inactivate-stack parser)
               t)
              ((is-image-close parser)
               (process-emphasis parser (1+ open-pos))
               ; () TODO make tag and push output queue
               (pop-stack-to-bottom parser open-pos)
               (inactivate-stack parser)
               t)
              (t nil))))))

;;
(defun is-link-close (parser)
  (let ((regex `(:sequence
                 :modeless-start-anchor
                 "]("
                 (:register
                  (:alternation
                   (:sequence
                    "<"
                    (:greedy-repetition 0 nil
                     (:inverted-char-class ,(code-char #x0a) ,(code-char #x0d)))
                    ">")
                   (:greedy-repetition 0 nil (:inverted-char-class #\) :whitespace-char-class))))
                 (:greedy-repetition 0 nil :whitespace-char-class)
                 (:register
                  (:greedy-repetition 0 1 
                   (:alternation
                    (:sequence "\"" (:regex "(?:\\\\\"|[^\"])*") "\"")
                    (:sequence "'" (:regex "(?:\\\\'|[^'])*") "'")
                    (:sequence "(" (:regex "(?:\\\\)|\\\\(|[^)(])*") ")"))))
                 ")")))
    ;; TODO
    (multiple-value-bind (result matches) (scan-to-strings parser regex)
      (if result
          (values result
                  (if (and (< 0 (length (aref matches 0)))
                           (char= (char (aref matches 0) 0) #\<))
                      (subseq (aref matches 0) 1 (1- (length (aref matches 0))))
                      (aref matches 0))
                  (if (< 0 (length (aref matches 1)))
                      (subseq (aref matches 1) 1 (1- (length (aref matches 1))))))))))
  
(defun is-image-close (parser)
  ;; TODO
  nil)

;;
(defun make-link (parser open-pos)
  ;; TODO
  (multiple-value-bind (result url title) (is-link-close parser)
    (replace-string parser
                    (format nil "<a href=\"~A\"~A>"
                            url
                            (if title
                                (format nil " title=\"~A\"" title)
                                ""))
                    open-pos (1+ open-pos))
    (pos+ parser (length result))
    (push-string parser "</a>")))

;;
(defun find-opening-delimiter (parser)
  (let ((stack (ip-stack parser)))
    (loop :for i :from (1- (length stack)) :downto 0
          :for delimiter := (aref stack i)
          :if (and (or (eq (dl-type delimiter) :|[|)
                       (eq (dl-type delimiter) :|![|))
                   (dl-active delimiter))
          :do (return-from find-opening-delimiter (values i delimiter)))))

(defun inactivate-stack (parser)
  (loop :for i :from (1- (length (ip-stack parser))) :downto 0
        :for delimiter := (aref (ip-stack parser) i)
        :if (or (eq (dl-type delimiter) :|[|)
                (eq (dl-type delimiter) :|![|))
        :do (setf (dl-active delimiter) nil)))

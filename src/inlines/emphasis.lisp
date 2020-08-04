(defpackage :clcm/inlines/emphasis
  (:use :cl
        :clcm/utils
        :clcm/characters
        :clcm/inlines/parser)
  (:export :scan-emphasis))
(in-package :clcm/inlines/emphasis)

(defun scan-emphasis (parser)
  (when (or (scan parser "^\\*") (scan parser "^_"))
    (or (close-*_ parser)
        (open-*_ parser))))

(defun *-can-open (parser)
  (or/mv (scan-to-strings parser
                          `(:sequence :modeless-start-anchor
                            (:regex "(\\*+)")
                            (:inverted-char-class  ,@`,(append *unicode-whitespaces* *punctuations*))))
         (if (/= (ip-position parser) 0)
             (scan-to-strings parser
                              `(:sequence :modeless-start-anchor
                                (:char-class
                                 ,@`,(append *unicode-whitespaces* *punctuations-without-**))
                                (:regex "(\\*+)")
                                (:char-class ,@*punctuations-without-**))
                              :offset -1)
             (scan-to-strings parser
                              `(:sequence :modeless-start-anchor
                                (:regex "(\\*+)")
                                (:char-class ,@*punctuations-without-**))))))

(defun _-can-open (parser)
  ;; TODO
  (or/mv (scan-to-strings parser
                          `(:sequence :modeless-start-anchor
                            (:regex "(_+)")
                            (:inverted-char-class  ,@`,(append *unicode-whitespaces* *punctuations*))))
         (if (/= (ip-position parser) 0)
             (scan-to-strings parser
                              `(:sequence :modeless-start-anchor
                                (:char-class
                                 ,@`,(append *unicode-whitespaces* *punctuations-without-**))
                                (:regex "(_+)")
                                (:char-class ,@*punctuations-without-**))
                              :offset -1)
             (scan-to-strings parser
                              `(:sequence :modeless-start-anchor
                                (:regex "(_+)")
                                (:char-class ,@*punctuations-without-**))))))

(defun open-*_ (parser)
  (multiple-value-bind (result s) (or/mv (*-can-open parser) (_-can-open parser))
    (when result
      (let* ((d (aref s 0)) ; d is delimiter run : ex. **
             (type (aref (aref s 0) 0))
             (start (fill-pointer (ip-queue parser)))
             (op (make-inline-op  :type type :start start :end (+ start (length d)) :num (length d))))
        (push-string parser d)
        (incf (ip-position parser) (length d))
        (push-op parser op)
        op))))

(defun *-can-close (parser)
  (if (= (ip-position parser) 0) (return-from *-can-close nil))
  (or/mv (scan-to-strings parser
                          `(:sequence :modeless-start-anchor
                            (:inverted-char-class ,@`,(append *unicode-whitespaces* *punctuations*))
                            (:regex "(\\*+)"))
                          :offset -1)
         (scan-to-strings parser
                          `(:sequence :modeless-start-anchor
                            (:char-class ,@*punctuations*)
                            (:regex "(\\*+)")
                            (:alternation
                             :modeless-end-anchor-no-newline
                             (:char-class
                              ,@`,(append *unicode-whitespaces* *punctuations-without-**))))
                          :offset -1)))

(defun _-can-close (parser)
  ;; TODO
  (if (= (ip-position parser) 0) (return-from _-can-close nil))
  (or/mv (scan-to-strings parser
                          `(:sequence :modeless-start-anchor
                            (:inverted-char-class ,@`,(append *unicode-whitespaces* *punctuations*))
                            (:regex "(_+)"))
                          :offset -1)
         (scan-to-strings parser
                          `(:sequence :modeless-start-anchor
                            (:char-class ,@*punctuations*)
                            (:regex "(_+)")
                            (:alternation
                             :modeless-end-anchor-no-newline
                             (:char-class
                              ,@`,(append *unicode-whitespaces* *punctuations-without-**))))
                          :offset -1)))

(defun close-*_ (parser)
  (multiple-value-bind (result s) (or/mv (*-can-close parser) (_-can-close parser))
    (when result
      (let* ((d (aref s 0)) ; d is delimiter run : ex. **
             (type (aref (aref s 0) 0))
             (op (pop-op parser type)))
        (cond ((null op) nil)
              ; NOTE incf do not return nil
              ((= (iop-num op) (length d))
               (*_->em-strong parser (length d) (iop-start op))
               (incf (ip-position parser) (length d)))
              ((> (iop-num op) (length d))
               (let ((remained (- (iop-num op) (length d))))
                 (*_->em-strong parser (length d) (+ (iop-start op) remained))
                 (setf (iop-num op) remained)
                 (setf (iop-end op) (+ (iop-start op) remained))
                 (push-op parser op)
                 (incf (ip-position parser) (length d))))
              ((< (iop-num op) (length d))
               (let ((remained (- (length d) (iop-num op))))
                 (*_->em-strong parser (iop-num op) (iop-start op))
                 (incf (ip-position parser) (- (length d) remained)))))))))

(defun *_->em-strong (parser len start) ; consume *** => len = 3
  (multiple-value-bind (s-num e-num) (floor len 2)
    (when (= 1 e-num)
      (replace-string parser "<em>" start (+ start 1))
      (push-string parser "</em>"))
    (loop :for i :from 0 :to (1- s-num)
          :do (replace-string parser "<strong>"
                              (+ start (* e-num 4) (* 2 i))
                              (+ start (* e-num 4) (* 2 i) 2)))
    (loop :for i :from 0 :to (1- s-num)
          :do (push-string parser "</strong>"))))



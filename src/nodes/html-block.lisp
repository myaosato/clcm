(defpackage :clcm/nodes/html-block
  (:use :cl :clcm/node)
  (:import-from :cl-ppcre
                :scan)
  (:import-from :clcm/line
                :is-blank-line)
  (:export :html-block-node))
(in-package :clcm/nodes/html-block)

(defclass html-block-node (node)
  ((block-type :accessor block-type :initarg :block-type)
   ;; ref. https://spec.commonmark.org/0.29/#html-blocks
   ))

(defmethod close!? ((node html-block-node) line)
  (if (or (and (= (block-type node) 6) (is-blank-line line))
          (and (= (block-type node) 7) (is-blank-line line)))
      (close-node node)))

(defmethod add!? ((node html-block-node) line)
  (add-child node line)
  (if (or (and (= (block-type node) 1) (scan "</(?:script|pre|style)>" line))
          (and (= (block-type node) 2) (scan "-->" line))
          (and (= (block-type node) 3) (scan "\\?>" line))
          (and (= (block-type node) 4) (scan ">" line))
          (and (= (block-type node) 5) (scan "]]>" line)))
      (close-node node)))

(defmethod ->html ((node html-block-node))
  (let ((content (format nil "~{~A~^~%~}" (reverse (children node)))))
    (format nil "~A~%" content)))

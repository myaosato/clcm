(defpackage :clcm/nodes/html-block
  (:use :cl
        :clcm/line
        :clcm/raw-html-regex
        :clcm/node)
  (:import-from :cl-ppcre
                :scan)
  (:export :html-block-node
           :is-html-block-line
           :attach-html-block!?))
(in-package :clcm/nodes/html-block)

(defclass html-block-node (node)
  ((block-type :accessor block-type :initarg :block-type)
   ;; ref. https://spec.commonmark.org/0.29/#html-blocks
   ))

;; close
(defmethod close!? ((node html-block-node) line offset)
  (declare (ignore offset))
  (if (or (and (= (block-type node) 6) (is-blank-line line))
          (and (= (block-type node) 7) (is-blank-line line)))
      (close-node node)))

;; add
(defmethod add!? ((node html-block-node) line offset)
  (declare (ignore offset))
  (add-child node line)
  (if (or (and (= (block-type node) 1) (scan "</(?:script|pre|style)>" line))
          (and (= (block-type node) 2) (scan "-->" line))
          (and (= (block-type node) 3) (scan "\\?>" line))
          (and (= (block-type node) 4) (scan ">" line))
          (and (= (block-type node) 5) (scan "]]>" line)))
      (close-node node)))

;; ->html
(defmethod ->html ((node html-block-node))
  (let ((content (format nil "~{~A~^~%~}" (children node))))
    (format nil "~A~%" content)))


;;
;; ref. https://spec.commonmark.org/0.29/#html-blocks
(defun is-html-block-type-1-start-line (line)
  (scan "^ {0,3}(?:<script|<pre|<style)[\\s>$]" line))

(defun is-html-block-type-2-start-line (line)
  (scan "^ {0,3}<!--" line))

(defun is-html-block-type-3-start-line (line)
  (scan "^ {0,3}<\\?" line))

(defun is-html-block-type-4-start-line (line)
  (scan "^ {0,3}<![A-Z]" line))

(defun is-html-block-type-5-start-line (line)
  (scan "^ {0,3}<!\\[CDATA\\[" line))

(defvar *html-block-type-6-regex*
  '(:sequence
    :start-anchor
    (:greedy-repetition 0 3 " ")
    (:alternation "<" "</")
    (:alternation "address" "article" "aside"
     "base" "basefont" "blockquote" "body"
     "caption" "center" "col" "colgroup"
     "dd" "details" "dialog" "dir" "div" "dl" "dt"
     "fieldset" "figcaption" "figure" "footer" "form" "frame" "frameset"
     "h1" "h2" "h3" "h4" "h5" "h6" "head" "header" "hr" "html"
     "iframe" "legend" "li" "link" "main" "menu" "menuitem"
     "nav" "noframes" "ol" "optgroup" "option" "p" "param"
     "section" "source" "summary"
     "table" "tbody" "td" "tfoot" "th" "thead" "title" "tr" "track" "ul")
    (:alternation :whitespace-char-class :end-anchor  ">" "/>")))

(defun is-html-block-type-6-start-line (line)
  (scan *html-block-type-6-regex* line))

(defvar *html-block-type-7-regex*
  `(:sequence
    :start-anchor
    (:greedy-repetition 0 3 " ")
    (:alternation ,*open-tag* ,*closing-tag*)
    (:greedy-repetition 0 nil :whitespace-char-class)))

(defun is-html-block-type-7-start-line (line)
  (scan *html-block-type-7-regex* line))

(defun is-html-block-line (types line)
  (unless (listp types)
    (setf types (list types)))
  (loop :for type :in types
        :if (%is-html-block-line type line)
        :do (return-from is-html-block-line t))
  nil)

(defun %is-html-block-line (type line)
  (case type
    (1 (is-html-block-type-1-start-line line))
    (2 (is-html-block-type-2-start-line line))
    (3 (is-html-block-type-3-start-line line))
    (4 (is-html-block-type-4-start-line line))
    (5 (is-html-block-type-5-start-line line))
    (6 (is-html-block-type-6-start-line line))
    (7 (is-html-block-type-7-start-line line))
    (otherwise nil)))

(defun attach-html-block!? (node line)
  (loop :for type :from 1 :to 7
        :if (is-html-block-line type line)
        :do (let ((child (make-instance 'html-block-node :block-type type)))
              (add-child node child)
              (add!? child line 0)
              (return-from attach-html-block!? child)))
  nil)

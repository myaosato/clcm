(defpackage :clcm/nodes/html-block
  (:use :cl :clcm/node)
  (:import-from :cl-ppcre
                :scan)
  (:import-from :clcm/line
                :is-blank-line)
  (:import-from :clcm/raw-html-regex
                :*open-tag*
                :*closing-tag*)
  (:export :html-block-node
           :is-html-block-type-1-start-line
           :is-html-block-type-2-start-line
           :is-html-block-type-3-start-line
           :is-html-block-type-4-start-line
           :is-html-block-type-5-start-line
           :is-html-block-type-6-start-line
           :is-html-block-type-7-start-line))
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

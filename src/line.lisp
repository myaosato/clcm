(defpackage :clcm/line
  (:use :cl)
  (:import-from :cl-ppcre
                :scan
                :scan-to-strings)
  (:import-from :clcm/raw-html-regex
                :*open-tag*
                :*closing-tag*)
  (:export :string->lines
           :is-blank-line
           :is-thematic-break-line
           :is-atx-heading-line
           :is-setext-heading-level-1-line
           :is-setext-heading-level-2-line
           :is-indented-code-block-line
           :is-indented-code-block-close-line
           :is-backtick-fenced-code-block-line
           :is-tilde-fenced-code-block-line
           :is-html-block-type-1-start-line
           :is-html-block-type-2-start-line
           :is-html-block-type-3-start-line
           :is-html-block-type-4-start-line
           :is-html-block-type-5-start-line
           :is-html-block-type-6-start-line
           :is-html-block-type-7-start-line
           :is-block-quote-line
           :*white-space-characters*))
(in-package :clcm/line)

;; utils
(defvar *white-space-characters* (mapcar #'code-char '(#x20 #x09 #x0A #x0B #x0C #x0D)))

(defun string->lines (input)
  (lines input 0 nil))

(defun lines (input pos output)
  (if (>= pos (length input))
      (reverse output)
      (line input pos output nil)))

(defun line (input pos output buf)
  (labels ((buf->line (chars) (concatenate 'string (reverse chars))))
    (if (>= pos (length input))
        (return-from line  (lines input pos (if buf (cons (buf->line buf) output)))))
    (let ((char (char input pos)))
      (cond ((char= char (code-char 10))
             (lines input (1+ pos) (cons (buf->line buf) output)))
            ((char= char (code-char 13))
             (if (char= (char input (1+ pos)) (code-char 10))
                 (lines input (+ 2 pos) (cons (buf->line buf) output))
                 (lines input (1+ pos) (cons (buf->line buf) output))))
            (t (line input (1+ pos) output (cons char buf)))))))

;; blocks ;;

;; blank line
(defun is-blank-line (line)
  (scan `(:sequence 
          :start-anchor
          (:greedy-repetition 0 nil (:alternation ,(code-char #x20) ,(code-char #x09)))
          :end-anchor)
        line))

;; themantic break
(defun is-thematic-break-line (line)
  (or (scan "^ {0,3}(?:\\*\\s*){3,}$" line)
      (scan "^ {0,3}(?:_\\s*){3,}$" line)
      (scan "^ {0,3}(?:-\\s*){3,}$" line)))

;; atx heading
(defun is-atx-heading-line (line)
  (scan "^ {0,3}#{1,6}(\\t| |$)" line))

;; setext heading
(defun is-setext-heading-level-1-line (line)
  (scan "^ {0,3}=+\\s*" line))

(defun is-setext-heading-level-2-line (line)
  (scan "^ {0,3}-+\\s*" line))

;; indented code block
(defun is-indented-code-block-line (line)
  (scan "^(?: {0,3}\\t|    )" line))

(defun is-indented-code-block-close-line (line)
  (scan "^ {0,3}\\S" line))

;; fenced code block
(defun is-backtick-fenced-code-block-line (line)
  (scan-to-strings "^( {0,3})(`{3,})\\s*([^`\\s]*)(?:\\s[^`]*)?$" line))

(defun is-tilde-fenced-code-block-line (line)
  (scan-to-strings "^( {0,3})(~{3,})\\s*([^\\s]*)(?:\\s.*)?$" line))


;; HTML block
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

;; block quote
(defvar *block-quote-marker*
  `(:sequence
    :start-anchor
    (:greedy-repetition 0 3 " ")
    ">"))

(defun is-block-quote-line (line)
  (scan *block-quote-marker* line))

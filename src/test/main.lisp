(defpackage :clcm/test/main
  (:nicknames :clcm/test)
  (:use :cl)
  (:import-from :clcm/inlines)
  (:export :test))
(in-package :clcm/test/main)

(defun test ()
  (assert (equal (clcm/inlines:parse-inline "![aaa](hoge)")
                 '((:IMAGE ("aaa") "hoge" NIL))))
  (assert (equal (clcm/inlines:parse-inline "![aaa](foo \"bar\")")
                 '((:IMAGE ("aaa") "foo" "bar"))))
  (assert (equal (clcm/inlines:parse-inline "[hoge](foo (bar))")
                 '((:LINK ("hoge") "foo" "bar"))))
  (assert (equal (clcm/inlines:parse-inline "[hoge](foo)")
                 '((:LINK ("hoge") "foo" NIL))))
  (assert (equal (clcm/inlines:parse-inline "![foo](bar 'baz')")
                 '((:IMAGE ("foo") "bar" "baz"))))
  (assert (equal (clcm/inlines:parse-inline "[![foo](bar 'baz')](hoge 'piyo')")
                 '((:LINK ((:IMAGE ("foo") "bar" "baz")) "hoge" "piyo"))))


  )


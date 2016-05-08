;;;; dom-test.lisp
;;;;
;;;; CL-USER> (lisp-unit2:with-failure-debugging ()
;;;;            (lisp-unit2:run-tests :package :struct-wrapper.dom))

(in-package :struct-wrapper.dom)

;;; ---------- Parsers ----------

(define-test dom-from-html-test ()
  (assert-equal '("html" ()
                  ("head" ())
                  ("body" ()
                   ("a" (("href" "link")) "haha")
                   ("ul" ()
                    ("li" () "1")
                    ("li" () "2"))))
                (dom-from-html "<body><a href=\"link\">haha</a><ul><li>1</li><li>2</li></ul></body>")))
  
(define-test dom-attribute-test ()
  (assert-equal "link"
                (dom-attribute '("a" (("id" "ccd")
                                      ("href" "link"))
                                 "Caption")
                               "href"))
  (assert-equal nil
                (dom-attribute '("a" (("id" "ccd")
                                      ("href" "link"))
                                 "Caption")
                               "class")))

(define-test dom-classes-test ()
  (assert-equal '("class1" "class2")
                (dom-classes '("a" (("class" " class1  class2  ")) "caption"))))

;;;; html-operation-test.lisp
;;;; Unit test for html-operation.lisp
;;;; Author: BreakDS <breakds@gmail.com>

(in-package #:breakds.html-operation-test)

(defsuite* (test-all :in root-suite
		     :documentation "tests for html operations"))

(deftest (get-tag-test 
	  :cases (("<a href=\"uri\">link</a>" "a")
		  ("plain text" nil)))
    (html-frag expected)
  (is (equal expected 
	     (get-tag (fragment-from-string html-frag)))))

(deftest (get-attributes-test
	  :cases (("<a href=\"uri\">link</a>" '(("href" "uri")))
		  ("plain text" nil)))
    (html-frag expected)
  (is (equal expected 
	     (get-attributes (fragment-from-string html-frag)))))

(deftest (get-class-test
	  :cases (("<a href=\"uri\">link</a>" nil)
		  ("<a href=\"uri\" class=\"funny tasty\">link</a>" 
		   '("funny" "tasty"))
		  ("plain text" nil)))
    (html-frag expected)
  (is (equal expected 
	     (get-class (fragment-from-string html-frag)))))

(deftest (get-children-test
	  :cases (("plain text" nil)
		  ("<ul><li>a1</li><li>a2</li></ul>"
		   '(("li" nil "a1") ("li" nil "a2")))
		  ("<head></head>" nil)))
    (html-frag expected)
  (is (equal expected 
	     (get-children (fragment-from-string html-frag)))))


    



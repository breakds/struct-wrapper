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

(deftest (get-content-test
	  :cases (("plain text" nil)
		  ("<ul><li>a1</li><li>a2</li></ul>"
		   "not-plain-text")
		  ("<a>haha</a>" "haha")))
    (html-frag expected)
  (is (equal expected 
	     (get-content (fragment-from-string html-frag)))))

(deftest (get-content-int-test
	  :cases (("plain text" nil)
		  ("<ul><li>a1</li><li>a2</li></ul>"
		   nil)
		  ("<a>18</a>" 18)
		  ("<a>18.1</a>" nil)
		  ("<a>haha</a>" nil)))
    (html-frag expected)
  (is (equal expected 
	     (get-content-int (fragment-from-string html-frag)))))
    
(deftest (match-pattern-test
	  :cases (("<A href=\"uri\">link</A>" "a" 0)
		  ("<a href=\"uri\">link</a>" "A:4" 4)
		  ("<a href=\"uri\" class=\"funny tasty\">link</a>"
		   ".funny" 0)))
    (html-frag head n-th)
  (is (not (null (match-pattern (fragment-from-string html-frag)
				head
				n-th)))))

(deftest (inverse-match-pattern-test
	  :cases (("<a href=\"uri\">link</a>" "b" 0)
		  ("<a href=\"uri\">link</a>" "a:3" 4)
		  ("<a href=\"uri\" class=\"funny tasty\">link</a>"
		   ".not-funny" 0)))
    (html-frag head n-th)
  (is (null (match-pattern (fragment-from-string html-frag)
			   head
			   n-th))))

(deftest (split-selector-test
	  :cases ((" a  ul .strong:4 v:1  "
		   "a"
		   "ul .strong:4 v:1")
		  ("" "" "")
		  ("a" "a" "")))
    (selector expected-head expected-tail)
  (multiple-value-bind (head tail) (split-selector selector)
    (is (string-equal expected-tail tail))
    (is (string-equal expected-head head))))


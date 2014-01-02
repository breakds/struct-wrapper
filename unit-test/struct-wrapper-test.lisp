;;;; struct-wrapper-test.lisp
;;;; Unit tests for struct-wrapper.lisp
;;;; Author: BreakDS <breakds@gmail.com>

(in-package #:breakds.struct-wrapper-test)

(defun node-from-test-html (uri)
  (html5-parser:node-to-xmls
   (html5-parser:parse-html5 
    (merge-pathnames (format nil "unit-test/~a" uri)
		     (asdf:system-source-directory 'struct-wrapper)))))

(defsuite* (test-all :in root-suite
		     :documentation "unit test for struct-wrapper"))

(deftest simple-struct-wrapper-test ()
  (let ((html (node-from-test-html "simple-struct-wrapper-test.1.html")))
    (is (equal (funcall (make-struct-wrapper 
			 ("body div h3" :name #'get-content)
			 ("body div ul .org" :org #'get-content)
			 ("body div ul .class" :class #'get-content)
			 ("body div ul .superior" 
			  :superior 
			  (lambda (node)
			    (if (string-equal (get-content node)
					      "yes")
				t nil)))
			 ("body div ul .miracle" :miracle #'get-content)
			 ("body div ul .rank" :rank #'get-content-int))
			html)
	       '(:miracle "Resurrection" :superior t :rank 10 
		 :org "Magi" :class "Summoner" 
		 :name "Van Senareos" :obj t)))))






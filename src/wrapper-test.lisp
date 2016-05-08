;;;; wrapper-test.lisp
;;;;
;;;; CL-USER> (lisp-unit2:with-failure-debugging ()
;;;;            (lisp-unit2:run-tests :package :struct-wrapper.wrapper))

(in-package :struct-wrapper.wrapper)

(define-test with-first-match-test ()
  (let ((node '("tr" ()
                ("td" () ("ul" ()
                          ("li" (("class" "class1")) "content1")
                          ("li" (("class" "class2")) "content2")))
                ("td" () ("ul" ()
                          ("li" (("class" "class2")) "content3")
                          ("li" (("class" "class2")) "content4"))))))
    (with-first-match (x :from node
                         :matches '("tr" "td" "ul" (:and "li" ".class2")))
      (assert-equal "content2" (first (dom-children x))))))

(define-test map-matches-test ()
  (let ((node '("tr" ()
                ("td" () ("ul" ()
                          ("li" (("class" "class1")) "content1")
                          ("li" (("class" "class2")) "content2")))
                ("td" () ("ul" ()
                          ("li" (("class" "class2")) "content3")
                          ("li" (("class" "class2")) "content4"))))))
    (assert-equal '("content2" "content3" "content4")
                  (map-matches (x :from node
                                  :matches '("tr" "td" "ul" (:and "li" ".class2")))
                    (first (dom-children x))))))
                    
      

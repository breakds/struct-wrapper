;;;; selector-test.lisp
;;;;
;;;; CL-USER> (lisp-unit2:with-failure-debugging ()
;;;;            (lisp-unit2:run-tests :package :struct-wrapper.selector))

(in-package :struct-wrapper.selector)

(define-test match-selector-test ()
  ;; :tag
  (assert-true (match-selector '("a" ())
                               "a"))
  (assert-false (match-selector '("a" ())
                                "b"))

  ;; :class
  (assert-true (match-selector '("a" (("class" "class1 class2 class3")))
                               ".class2"))
  (assert-false (match-selector '("a" (("class" "class1 class2 class3")))
                                ".class4"))

  ;; :id
  (assert-true (match-selector '("a" (("id" "some-id")
                                      ("class" "class1 class2")))
                               "#some-id"))
  (assert-false (match-selector '("a" (("id" "some-id")
                                       ("class" "class1 class2")))
                                "#another-id"))

  ;; :children/:thereis/:always
  (assert-true (match-selector '("tr" ()
                                 ("td" (("class" "class1 class2")))
                                 ("td" (("class" "class1 class3"))))
                               '(:children (:thereis ".class2"))))
  (assert-false (match-selector '("tr" ()
                                 ("td" (("class" "class1 class2")))
                                 ("td" (("class" "class1 class3"))))
                                '(:children (:always ".class2"))))
  (assert-true (match-selector '("tr" ()
                                 ("td" (("class" "class1 class2")))
                                 ("td" (("class" "class1 class3"))))
                               '(:children (:always (:and "td" ".class1")))))

  ;; :and/:or/:not/:t
  (assert-true (match-selector '("a" (("id" "some-id")
                                      ("class" "class1 class2")))
                               '(:and "#some-id" ".class1")))
  (assert-false (match-selector '("a" (("id" "some-id")
                                       ("class" "class1 class2")))
                                '(:or "#antoher-id" ".class3")))
  (assert-true (match-selector '("a" (("id" "some-id")
                                      ("class" "class1 class2")))
                               '(:not (:or "#antoher-id" ".class3"))))
  (assert-false (match-selector '("a" (("id" "some-id")
                                       ("class" "class1 class2")))
                                '(:not (:or t "#antoher-id" ".class3")))))

(define-test call-on-first-match-test ()
  (assert-equal "content2"
                (call-on-first-match '("tr" ()
                                       ("td" () ("ul" ()
                                                 ("li" (("class" "class1")) "content1")
                                                 ("li" (("class" "class2")) "content2")))
                                       ("td" () ("ul" ()
                                                 ("li" (("class" "class2")) "content3")
                                                 ("li" (("class" "class2")) "content4"))))
                                     '("tr" "td" "ul" (:and "li" ".class2"))
                                     (lambda (node)
                                       (first (dom-children node))))))

(define-test call-on-all-matches-test ()
  (assert-equal '("content2" "content3" "content4")
                (call-on-all-matches '("tr" ()
                                       ("td" () ("ul" ()
                                                 ("li" (("class" "class1")) "content1")
                                                 ("li" (("class" "class2")) "content2")))
                                       ("td" () ("ul" ()
                                                 ("li" (("class" "class2")) "content3")
                                                 ("li" (("class" "class2")) "content4"))))
                                     '("tr" "td" "ul" (:and "li" ".class2"))
                                     (lambda (node)
                                       (first (dom-children node))))))

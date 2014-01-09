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

(deftest (struct-equal-test 
          :cases (('(:obj t :a 12 :b "ohaha") 
                   '(:b "ohaha" :a 12 :obj t))
                  ('("a" "c" "b")
                   '("c" "b" "a"))
                  ("String" "String")
                  ('((:obj t :a 12 :b "heart")
                     (:obj t :c "this" :d "that"))
                   '((:obj t :c "this" :d "that")
                     (:obj t :a 12 :b "heart")))
                  ('(:obj t :a (:obj t :b "string") :c (1 2 3 "5"))
                   '(:a (:obj t :b "string") :c (1 2 3 "5") :obj t))))
    (a b)
  (is (struct-equal a b)))

(deftest (struct-not-equal-test 
          :cases (('(:obj t :a 12 :b "ohaha") 
                   '(:b "ohaha" :a 12))
                  ('("a" "c" "b")
                   '("c" "b" "t"))
                  (nil '(a b c))
                  ("String" "Stringx")
                  ('(:obj t :a (:obj t :b "string") :c 55)
                   '(:a (:obj t :b "string") :c (1 2 3 "5") :obj t))))
    (a b)
  (is (not (struct-equal a b))))

(deftest simple-struct-wrapper-test ()
  (let ((html (node-from-test-html "simple-struct-wrapper-test.1.html")))
    (is (struct-equal (funcall (make-struct-wrapper 
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

(deftest (simple-list-wrapper-test 
          :cases (("simple-list-wrapper-test.1.html"
                   '("Ace" "King" "Queen" "Jack"))))
    (test-html-file expected)
  (let ((html (node-from-test-html test-html-file)))
    (is (set-equal (funcall (make-list-wrapper
                             "body ol li"
                             #'get-content)
                            html)
                   expected))))

(deftest composite-test ()
  (let ((html (node-from-test-html "composite-test.html"))
        (wrapper (make-list-wrapper 
                  "body #content .hero"
                  (make-struct-wrapper
                   ("p b:1" :name #'get-content)
                   ("ul" :army (make-list-wrapper
                                "li:>1"
                                (make-struct-wrapper
                                 (".creature" :class #'get-content)
                                 (".quantity" :num #'get-content-int))))))))
    (let ((result (funcall wrapper html)))
      (is (struct-equal result
                        '((:obj t :name "Ivor" 
                           :army ((:obj t 
                                        :class "Centaur Captain"
                                        :num 100)
                                  (:obj t
                                        :class "Grand Elf"
                                        :num 45)
                                  (:obj t
                                        :class "Grand Elf"
                                        :num 150)))
                          (:obj t :name "Jenova" 
                           :army ((:obj t
                                        :class "War Unicorn"
                                        :num 5)
                                  (:obj t
                                        :class "Pegasus"
                                        :num 30)))
                          (:obj t :name "Kyrre" :army nil)))))))

;;; ---------- Tests for def versions ----------

(def-struct-wrapper army-unit-wrapper
  (".creature" :class #'get-content)
  (".quantity" :num #'get-content-int))

(def-list-wrapper army-wrapper
    "li:>1" #'army-unit-wrapper)

(def-struct-wrapper hero-wrapper
  ("p b:1" :name #'get-content)
  ("ul" :army #'army-wrapper))

(def-list-wrapper heroes-wrapper
    "body #content .hero" #'hero-wrapper)

(deftest def-wrapper-composite-test ()
  (let ((html (node-from-test-html "composite-test.html")))
    (is (struct-equal (heroes-wrapper html)
                      '((:obj t :name "Ivor" 
                         :army ((:obj t 
                                      :class "Centaur Captain"
                                      :num 100)
                                (:obj t
                                      :class "Grand Elf"
                                      :num 45)
                                (:obj t
                                      :class "Grand Elf"
                                      :num 150)))
                        (:obj t :name "Jenova" 
                         :army ((:obj t
                                      :class "War Unicorn"
                                      :num 5)
                                (:obj t
                                      :class "Pegasus"
                                      :num 30)))
                        (:obj t :name "Kyrre" :army nil))))))



        
    
                   
                  
                         







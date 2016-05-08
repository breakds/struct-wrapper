;;;; struct-wrapper.asd

(in-package :cl-user)
(defpackage struct-wrapper-asd
  (:use :cl :asdf))
(in-package :struct-wrapper-asd)

(defsystem #:struct-wrapper
  :serial t
  :depends-on (#:cl-html5-parser
               #:drakma
               #:cl-ppcre
               #:split-sequence
               #:alexandria
               #:lisp-unit2)
  :components ((:module "src"
                        :components
                        ((:file "dom")
                         (:file "dom-test"
                                :depends-on ("dom"))
                         (:file "selector")
                         (:file "selector-test"
                                :depends-on ("selector"))
                         (:file "wrapper")
                         (:file "wrapper-test"
                                :depends-on ("wrapper"))
                         (:file "struct-wrapper"
                                :depends-on ("dom" "selector" "wrapper")))))
  :description "Wrapper builder for web scraping.")

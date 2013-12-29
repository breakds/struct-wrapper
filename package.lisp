;;;; package.lisp

(defpackage #:breakds.html-operation
  (:nicknames #:html-operation)
  (:use #:cl
	#:swiss-knife
	#:split-sequence)
  (:export #:html-from-uri
	   #:html-from-string
	   #:fragment-from-string
	   ;; node accessors
	   #:is-trivial
	   #:get-tag
	   #:get-attributes
	   #:get-class
	   #:get-children
	   ;; descriptor related
	   #:get-selector
	   #:get-slot-name
	   #:get-callback
	   #:match-pattern
	   #:split-selector))

(defpackage #:breakds.struct-wrapper
  (:nicknames #:struct-wrapper)
  (:use #:cl
        #:ppcre
        #:swiss-knife)
  (:export #:match-selector-head
	   #:def-struct-wrapper
	   #:make-struct-wrapper))

;;; --- Unit Test Package

(defpackage #:breakds.html-operation-test
  (:nicknames #:html-operation-test)
  (:use #:cl
	#:stefil
	#:breakds.html-operation)
  (:export #:test-all))
	      
(defpackage #:breakds.struct-wrapper-test
  (:nicknames #:struct-wrapper-test)
  (:use #:cl 
	#:stefil 
	#:breakds.struct-wrapper)
  (:export #:test-all))


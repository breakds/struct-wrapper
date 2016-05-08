;;;; package.lisp

(defpackage #:breakds.html-operation
  (:nicknames #:html-operation)
  (:use #:cl
	#:split-sequence)
  (:import-from #:alexandria
                #:symbolicate)
  (:export #:html-from-uri
	   #:html-from-file
	   #:html-from-string
	   #:fragment-from-string
	   ;; node accessors
	   #:is-trivial
	   #:get-tag
	   #:get-attributes
           #:get-attribute
	   #:get-class
           #:get-id
	   #:get-children
	   #:get-content
	   #:get-content-int
	   ;; descriptor related
	   #:get-selector
	   #:get-slot-name
	   #:get-callback
           #:make-pattern-matcher
	   #:split-selector))

(defpackage #:breakds.struct-wrapper
  (:nicknames #:struct-wrapper)
  (:use #:cl
        #:ppcre
	#:html-operation)
  (:import-from #:alexandria
                #:symbolicate
                #:with-gensyms)
  (:export #:set-equal
           #:struct-equal
           #:empty-struct-p
           #:match-selector-head
	   #:def-struct-wrapper
	   #:make-struct-wrapper
           #:def-list-wrapper
           #:make-list-wrapper))

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
	#:breakds.html-operation
	#:breakds.struct-wrapper)
  (:export #:test-all))


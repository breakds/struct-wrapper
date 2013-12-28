;;;; package.lisp

(defpackage #:breakds.struct-wrapper
  (:nicknames #:struct-wrapper)
  (:use #:cl
        #:ppcre
	#:split-sequence
        #:swiss-knife)
  (:export #:match-selector-head
	   #:def-struct-wrapper
	   #:make-struct-wrapper))

;;; --- Unit Test Package

(defpackage #:breakds.struct-wrapper-test
  (:nicknames #:struct-wrapper-test)
  (:use #:cl 
	#:stefil 
	#:breakds.struct-wrapper)
  (:export #:test-all))


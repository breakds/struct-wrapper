;;;; package.lisp

(defpackage #:breakds.struct-wrapper
  (:nicknames #:struct-wrapper)
  (:use #:cl
        #:ppcre
        #:swiss-knife)
  (:export #:def-struct-wrapper))

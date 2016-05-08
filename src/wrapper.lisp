;;;; wrapper.lisp

(in-package :cl-user)
(defpackage struct-wrapper.wrapper
  (:use :cl :lisp-unit2)
  (:import-from #:alexandria
                #:with-gensyms)
  (:import-from #:struct-wrapper.dom
                #:dom-children)
  (:import-from #:struct-wrapper.selector
                #:call-on-first-match
                #:call-on-all-matches)
  (:export #:with-first-match
           #:map-matches))
(in-package :struct-wrapper.wrapper)

(defmacro with-first-match ((node-var &key (from nil) (matches nil)) &body body)
  `(call-on-first-match ,from ',matches
                        (lambda (,node-var)
                          ,@body)))

(defmacro map-matches ((node-var &key (from nil) (matches nil)) &body body)
  `(call-on-all-matches ,from ',matches
                        (lambda (,node-var)
                          ,@body)))

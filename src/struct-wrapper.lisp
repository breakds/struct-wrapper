;;;; struct-wrapper.lisp

(in-package :cl-user)
(defpackage struct-wrapper
  (:use :cl :lisp-unit2)
  (:import-from #:struct-wrapper.dom
                #:dom-from-html
                #:dom-from-uri

                #:dom-tag
                #:dom-attributes
                #:dom-attribute
                #:dom-children
                #:dom-classes)
  (:import-from #:struct-wrapper.wrapper
                #:with-first-match
                #:map-matches)
  (:export #:dom-from-html
           #:dom-from-uri

           #:dom-tag
           #:dom-attributes
           #:dom-attribute
           #:dom-children
           #:dom-classes
           
           #:with-first-match
           #:map-matches))
(in-package :struct-wrapper)

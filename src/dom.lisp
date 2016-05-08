;;;; dom.lisp
;;;;
;;;; Provides utilities that performas various operations on the
;;;; s-expression representation of DOM.

(in-package :cl-user)
(defpackage struct-wrapper.dom
  (:use :cl :lisp-unit2)
  (:import-from #:html5-parser
                #:transform-html5-dom
                #:parse-html5)
  (:export #:dom-from-html
           #:dom-from-uri

           #:dom-tag
           #:dom-attributes
           #:dom-attribute
           #:dom-children
           #:dom-classes))
(in-package :struct-wrapper.dom)

;;; ---------- Parsers ----------

(defun dom-from-html (html)
  "Convert the html string to the DOM representation."
  (transform-html5-dom :xmls
                       (parse-html5 html)))

(defun dom-from-uri (uri &key (encoding :utf-8))
  "Request the html from the uri and convert it to the DOM reprsentation."
  (dom-from-html (drakma:http-request uri
                                      :external-format-in encoding)))

;;; ---------- Accessors ----------

(declaim (inline dom-tag))
(defun dom-tag (node)
  "Returns the tag of the node."
  (declare (list node))
  (car node))

(declaim (inline dom-attributes))
(defun dom-attributes (node)
  "Returns the attributes list of the node."
  (declare (list node))
  (cadr node))

(declaim (inline dom-attribute))
(defun dom-attribute (node attribute)
  (cadr (assoc attribute (dom-attributes node)
               :test #'string=)))

(declaim (inline dom-children))
(defun dom-children (node)
  "Returns the children list of the node."
  (declare (list node))
  (cddr node))

(defun dom-classes (node)
  (let ((classes (dom-attribute node "class")))
    (when (stringp classes)
      (remove-if (lambda (x) (string= x ""))
                 (split-sequence:split-sequence #\SPACE classes)))))

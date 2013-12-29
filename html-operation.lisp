;;;; html-operation.lisp
;;;; Operations to select attributes, content, tag and children of
;;;; s-expression represented html5 nodes. It mainly provides
;;;; asscessors to s-exp representation slots and CSS-like selectors
;;;; operations.
;;;; Author: BreakDS <breakds@gmail.com>

(in-package #:html-operation)

;; 

(defun html-from-string (str)
  "Convert the html text to the s-exp representation."
  (html5-parser:node-to-xmls
   (html5-parser:parse-html5 str)))

(defun html-from-uri (uri)
  "Convert the html text to the s-exp representation, where the html
  text comes from an uri."
  (node-from-string (drakma:http-request uri)))

(defun fragment-from-string (str)
  "Convert the text containing the html fragment to its corresponding
  s-exp representation."
  (car (html5-parser:node-to-xmls
	(html5-parser:parse-html5-fragment str))))

(defun is-trivial (node)
  "Trivial nodes are defined to be the plain string. They do not have
  tags, attributes and children, and thus their s-exp representation
  is just a plain string rather than a list (cons)"
  (not (consp node)))

(defmacro def-node-accessor (property-name &body body)
  "This macro is used to define accessors to properties of s-exp
  represented htmt5 nodes. Accessors defined in this way accept a
  parameter which is the node, and returns nil if the node is
  trivial."
  ;; This anaphoric macro force the parameter to be node. This is fine
  ;; since the macro will only be used internally in this package.
  `(defun ,(symb 'get- property-name) (node)
     (unless (is-trivial node)
       ,@body)))

(declaim (inline get-tag))
(def-node-accessor tag 
    (car node))

(declaim (inline get-attributes))
(def-node-accessor attributes
    (cadr node))

(declaim (inline get-class))
(def-node-accessor class
  (let ((result (split-sequence #\space 
				(cadr (assoc "class" 
					     (get-attributes node)
					     :test #'equal)))))
    (when (car result)
      result)))


(declaim (inline get-children))
(def-node-accessor children
    (cddr node))
    


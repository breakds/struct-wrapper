;;;; html-operation.lisp
;;;; Operations to select attributes, content, tag and children of
;;;; s-expression represented html5 nodes. It also provides
;;;; asscessors to slot descriptors.

;;;; Author: BreakDS <breakds@gmail.com>

;;;; A slot descriptor is a triplet of (selector slot callback), where
;;;; 1. selector is a CSS-like selector that identifies a node.
;;;; 2. slot is a keyword symbol that stands for the slot name.
;;;; 3. callback is a function on the selected node that returns an
;;;; object.

;;;; A slot descriptor like (selector slot callback) denotes the
;;;; operations of applying the callback on the node that matches the
;;;; selector, and assign the return value to the specified slot.

(in-package #:html-operation)

;;; --- s-exp node operations

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


;;; --- Slot descriptor operations

(declaim (inline get-selector))
(defun get-selector (descriptor)
  (car descriptor))

(declaim (inline get-slot-name))
(defun get-slot-name (descriptor)
  (cadr descriptor))

(declaim (inline get-callback))
(defun get-callback (descriptor)
  (caddr descriptor))

(declaim (inline analyze-pattern))
(defun analyze-pattern (pattern)
  (let ((splitted (split-sequence #\: pattern)))
    (values (car splitted)
	    (handler-case 
		(parse-integer (cadr splitted))
	      (t nil)))))

(defun match-pattern (node head &optional (n-th 0))
  "Return T if the node matches the selector head."
  ;; selectors is usually a list of selector patterns, and head is the
  ;; first one. Currently supported patterns are: "tag-name"
  ;; ".class-name" and "[tag-name|.class-name]:n", where n stands for
  ;; we want the n-th match.
  (multiple-value-bind (major postfix) (analyze-pattern head)
    (when (or (null postfix) (= postfix n-th))
      (cond ((eq (aref major 0) #\.) (member (subseq major 1)
					     (get-class node)
					     :test #'string-equal))
	    (t (string-equal major (get-tag node)))))))

(defun split-selector (selector)
  "split the selector string into two parts (as values) and return
  them, where the first one is the first selector, and the second one
  are the rest of the selector. The second part may be nil."
  (let* ((stripped (string-trim " " selector))
         (i (position #\space stripped)))
    (if i
        (values (subseq stripped 0 i)
                (string-trim " " (subseq stripped (1+ i))))
        (values stripped ""))))



    

  

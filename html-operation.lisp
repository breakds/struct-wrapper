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
  ;; one thing that is worth being noted here is that by converting a
  ;; string into a html node, html5-parser will automatically
  ;; normalize it. That says, if the string being parsed does not come
  ;; with <thml> <head> etc., it will wrap them in
  ;; <html><body></body></html>.
  (html5-parser:node-to-xmls
   (html5-parser:parse-html5 str)))

(defun html-from-uri (uri)
  "Convert the html text to the s-exp representation, where the html
  text comes from an uri."
  (html-from-string (drakma:http-request uri)))

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
					     :test #'string-equal)))))
    (when (car result)
      result)))

(declaim (inline get-id))
(def-node-accessor id
  (let ((result (cadr (assoc "id"
                             (get-attributes node)
                             :test #'string-equal))))
    (when result
      result)))


(declaim (inline get-children))
(def-node-accessor children
  (cddr node))

(declaim (inline get-content))
(def-node-accessor content
  (let ((holder (caddr node)))
    (if (is-trivial holder)
	holder
	"not-plain-text")))

(declaim (inline get-content-int))
(def-node-accessor content-int
  (let ((content (get-content node)))
    (when (stringp content)
      (handler-case (parse-integer content)
        (t () nil)))))




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
  "Split the pattern into three parts: the part before the colon is
  called the major, and the part after the colon (if colon exists) is
  called the postfix, which should be an integer, possibly with a
  prefix operator >, <."
  (let* ((splitted (split-sequence #\: pattern))
         (major (car splitted))
         (post-major (cadr splitted))
         postfix comparator)
    (when (and post-major
               (> (length post-major) 0))
      (case (aref post-major 0)
        (#\> (setf postfix (subseq post-major 1))
             (setf comparator #'>))
        (#\< (setf postfix (subseq post-major 1))
             (setf comparator #'<))
        (t (setf postfix post-major)
           (setf comparator #'=))))
    (values major comparator 
            (handler-case 
		(parse-integer postfix)
              (t nil)))))

(declaim (inline match-class-pattern))
(defun match-class-pattern (node pattern)
  "Return T if the class attribute of the node contains the provided
  pattern."
  (member (subseq pattern 1) (get-class node) :test #'string-equal))

(declaim (inline match-tag-pattern))
(defun match-tag-pattern (node pattern)
  "Return T if the tag of the node contains the provided pattern."
  (string-equal pattern (get-tag node)))

(declaim (inline match-id-pattern))
(defun match-id-pattern (node pattern)
  "Return T if the id of the node matches the pattern."
  (string-equal (subseq pattern 1) (get-id node)))

(defun make-pattern-matcher (head)
  "Create a lambda that accepts a node. If the node matches the
  provided selector head, the lambda will return T." 
  ;; Currently supported patterns are: "tag-name" ".class-name" and
  ;; "[tag-name|.class-name]:n", where n stands for we want the n-th
  ;; match.
  (multiple-value-bind (major comparator postfix) (analyze-pattern head)
    (let ((is-class-pattern (eq (aref major 0) #\.))
          (is-id-pattern (eq (aref major 0) #\#)))
      (if (null postfix)
          (cond (is-class-pattern 
                 (lambda (node)
                   (match-class-pattern node major)))
                (is-id-pattern
                 (lambda (node)
                   (match-id-pattern node major)))
                (t (lambda (node)
                     (match-tag-pattern node major))))
          (let ((counter 0))
            (cond (is-class-pattern
                   (lambda (node)
                     (when (match-class-pattern node major)
                       (incf counter)
                       (funcall comparator counter postfix))))
                  (is-id-pattern
                   (lambda (node)
                     (when (match-id-pattern node major)
                       (incf counter)
                       (funcall comparator counter postfix))))
                  (t (lambda (node)
                       (when (match-tag-pattern node major)
                         (incf counter)
                         (funcall comparator counter postfix))))))))))

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







;;;; selector.lisp
;;;;
;;;; Selector library implements the CSS-like selectors and their
;;;; matching logics.

(in-package :cl-user)
(defpackage struct-wrapper.selector
  (:use :cl :lisp-unit2)
  (:import-from #:struct-wrapper.dom
                ;; Accessors
                #:dom-tag
                #:dom-attributes
                #:dom-attribute
                #:dom-children
                #:dom-classes)
  (:export #:match-selector
           #:call-on-first-match
           #:call-on-all-matches))
(in-package :struct-wrapper.selector)

(defun formalize-selector (selector)
  "Translate some of the CSS shorthand into selector expression."

  (cond ((stringp selector)
         (cond ((eq (char selector 0) #\#)
                `(:id (:= ,(subseq selector 1))))
               ((eq (char selector 0) #\.)
                `(:class (:has ,(subseq selector 1))))
               (t `(:tag (:= ,selector)))))
        ((integerp selector)
         `(:= ,selector))
        ((eq selector t)
         `(:t))
        (t selector)))

(defun match-selector (element input-selector)
  "Match the selector against element, and return T if they do match."
  ;; Element can be either a node, a string, a number or a list.
  (let ((selector (formalize-selector input-selector)))
    (when (and (listp selector)
               (keywordp (car selector)))
      (let ((operator (first selector))
            (operands (rest selector)))
        (unless (and (member operator '(:id :class :tag :children))
                     (not (listp element)))
          (case (car selector)
            (:id (match-selector (dom-attribute element "id")
                                 (first operands)))
            (:class (match-selector (dom-classes element)
                                    (first operands)))
            (:tag (match-selector (dom-tag element)
                                  (first operands)))
            (:children (match-selector (dom-children element)
                                       (first operands)))
            (:= (if (integerp (first operands))
                    (= element (first operands))
                    (string= element (first operands))))
            (:always (loop for sub-element in element
                        always (match-selector sub-element (first operands))))
            (:thereis (loop for sub-element in element
                         thereis (match-selector sub-element (first operands))))
            (:has (member (first operands) element
                          :test #'string=))
            (:t t)
            (:and (loop for operand in operands
                     always (match-selector element operand)))
            (:or (loop for operand in operands
                    thereis (match-selector element operand)))
            (:not (not (match-selector element (first operands))))))))))

(defun call-on-first-match (node selectors func)
  (if (null selectors)
      (funcall func node)
      (when (match-selector node (first selectors))
        (if (null (rest selectors))
            (funcall func node)
            (loop
               for child in (dom-children node)
               for match = (call-on-first-match child (rest selectors) func)
               when match
               return match)))))

(defun call-on-all-matches (node selectors func)
  (let (accu)
    (labels ((call-accu (node selectors)
               (if (null selectors)
                   (list (funcall func node))
                   (when (match-selector node (first selectors))
                     (if (null (rest selectors))
                         (push (funcall func node) accu)
                         (loop
                            for child in (dom-children node)
                            do (call-accu child (rest selectors))))))))
      (call-accu node selectors)
      (nreverse accu))))

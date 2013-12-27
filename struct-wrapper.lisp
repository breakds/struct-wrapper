;;;; struct-wrapper.lisp
;;;; author: BreakDS <breakds@gmail.com>

(in-package #:struct-wrapper)

(defun node-from-uri (uri)
  "Get the s-expression representation of the targeted webpage"
  (html5-parser:node-to-xmls 
   (html5-parser:parse-html5 
    (drakma:http-request uri))))


;;; ---------- Aux Subroutines for slot descriptors ----------

(defun get-selector (slot-descriptor)
  (car slot-descriptor))

(defun get-slot-name (slot-descriptor)
  (cadr slot-descriptor))

(defun get-callback (slot-descriptor)
  (caddr slot-descriptor))

;;; ---------- Aux Subroutines for s-expression node ----------

(defun get-children (node)
  "Fetch the list of children in an s-expression node."
  (cddr node))

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

(defun cluster-by-selector-head (slot-descriptors)
  "Cluster the slot-descriptors by the heads of the selectors
associated with each descriptor. Descriptor with an empty-string
selector head will be clustered as leaves."
  (let ((clusters nil)
        (leaves nil))
    (loop for slot-descriptor in slot-descriptors
       do (multiple-value-bind (head tail) 
              (split-selector (get-selector slot-descriptor))
            (if (equal head "")
                (push slot-descriptor leaves)
                (push (cons tail (rest slot-descriptor))
                      (getf clusters head)))))
    (values clusters leaves)))

;;; ---------- Struct Wrapper Macros ----------

(defun build-wrapper-lambda (slot-descriptors result-name)
  (with-gensyms (node child)
    (multiple-value-bind (clusters leaves)
        (cluster-by-selector-head slot-descriptors)
      `(lambda (,node)
         ,@(let ((clauses 
                  (when clusters
                    `(loop for ,child in (get-children ,node)
                        do ,@(loop for (head sub-descriptors) in clusters
                                collect `(when (match-selector-head 
                                                ,child 
                                                ,head)
                                           (funcall ,(build-wrapper-lambda
                                                      sub-descriptors
                                                      result-name)
                                                    ,child)))))))
                (when leaves
                  (loop for descriptor in leaves
                     do (push `(setf (getf ,result-name
                                           ,(get-slot-name descriptor))
                                     (funcall ,(get-callback descriptor)
                                              ,node))
                              clauses)))
                clauses)))))


(defmacro def-struct-wrapper (name &rest slot-descriptors)
  (with-gensyms (result node)
    `(defun ,name (,node)
       (let ((,result '(:obj t)))
         (funcall ,(build-wrapper-lambda slot-descriptors result)
                  ,node)))))

(defmacro test-def (&rest slot-descriptors)
  (multiple-value-bind (a b) 
      (cluster-by-selector-head slot-descriptors)
    `(,a ,b)))








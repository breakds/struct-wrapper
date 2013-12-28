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

(defun is-node (node)
  (consp node))

(defun get-attributes (node)
  (when (is-node node)
    (cadr node)))

(defun get-class (node)
  (when (is-node node)
    (cdr (assoc "class" (get-attributes node)
		:test #'equal))))

(defun get-tag (node)
  (when (is-node node)
    (car node)))

(defun match-selector-head (node head)
  (cond ((eq (aref head 0) #\.) (member (subseq head 1)
					(get-class node)
					:test #'string-equal))
	(t (string-equal head (get-tag node)))))

      

(defun get-children (node)
  "Fetch the list of children in an s-expression node."
  (when (is-node node)
    (cddr node)))

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
  (let ((clusters (make-hash-table :test #'equal))
        (leaves nil))
    (loop for slot-descriptor in slot-descriptors
       do (multiple-value-bind (head tail) 
              (split-selector (get-selector slot-descriptor))
            (if (equal head "")
                (push slot-descriptor leaves)
                (push (cons tail (rest slot-descriptor))
		      (gethash head clusters)))))
    (values clusters leaves)))

;;; ---------- Struct Wrapper Macros ----------

(defun build-wrapper-lambda (slot-descriptors result-name)
  (with-gensyms (node child)
    (multiple-value-bind (clusters leaves)
        (cluster-by-selector-head slot-descriptors)
      `(lambda (,node)
         ,@(let ((clauses 
                  (unless (zerop (hash-table-count clusters))
                    `((loop for ,child in (get-children ,node)
                        do ,@(loop 
				for head being 
				the hash-keys of clusters
				for sub-descriptors being 
				the hash-values of clusters
                                collect `(when (match-selector-head 
                                                ,child 
                                                ,head)
                                           (funcall ,(build-wrapper-lambda
                                                      sub-descriptors
                                                      result-name)
                                                    ,child))))))))
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
                  ,node)
	 ,result))))

(defmacro make-struct-wrapper (&rest slot-descriptors)
  (with-gensyms (node result)
    `(lambda (,node)
       (let ((,result '(:obj t)))
	 (funcall ,(build-wrapper-lambda slot-descriptors result)
                  ,node)
	 ,result))))










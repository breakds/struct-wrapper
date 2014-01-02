;;;; struct-wrapper.lisp
;;;; author: BreakDS <breakds@gmail.com>

(in-package #:struct-wrapper)

;;; ---------- Aux subroutines for slot descriptors ----------

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
                                collect `(when (match-pattern 
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

;; (defmacro def-list-wrapper (name selector callback)
;;   (with-gensym (result)
;;     `(defun ,name (,node)
;;        (let ((,result nil))

(defun build-list-wrapper-lambda (selector callback result-name)
  (with-gensyms (node child)
    (multiple-value-bind (head tail)
	(split-selector selector)
      `(lambda (,node)
	 ,(if (equal head "")
	      `(push (funcall ,callback ,node)
		     ,result-name)
	      `((loop for ,child in (get-children ,node)
		   do (when (match-pattern 
			     ,child 
			     ,head)
			(funcall ,(build-list-wrapper-lambda
				   tail
				   callback
				   result-name)
				 ,child)))))))))









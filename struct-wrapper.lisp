;;;; struct-wrapper.lisp
;;;; author: BreakDS <breakds@gmail.com>

(in-package #:struct-wrapper)

;;; ---------- Aux subroutines for p-list struct representation ----------
;;; The p-list struct representation is demonstrated below:
;;; '(:slot-name-1 ...
;;;   :slot-name-2 ...
;;;   ....
;;;   :obj t)
;;; the list will be treated as a struct iff :obj is presented and set to true.
(defun set-equal (list-a list-b &key (test #'equal))
  "Return t if list-a and list-b has exactly the same
  arguments (Equality test for unordred list)."
  (null (if (> (length list-a) (length list-b)) 
            (set-difference list-a
                            list-b
                            :test test)
            (set-difference list-b
                            list-a
                            :test test))))

(defun struct-equal (obj-a obj-b)
  "Equality test for obj-a and obj-b. If both are structs, return t
  when every slot of them are struct-equal. If both are lists, return
  t when they are set-equal on struct-equal. If both are
  elements (string, integer etc.), return t when they are
  equal. Return nil otherwise."
  (labels ((try-identify-struct (obj)
             (handler-case (getf obj :obj)
               (t () nil))))
    (if (and (consp obj-a)
             (consp obj-b))
        (cond ((and (try-identify-struct obj-a)
                    (try-identify-struct obj-b))
               ;; check whether obj-a and obj-b disagree on at
               ;; least one of obj-a's keys
               (unless (loop for key in obj-a by #'cddr
                          when (not (struct-equal (getf obj-a key)
                                                  (getf obj-b key)))
                          return t)
                 t))
              (t (set-equal obj-a obj-b :test #'struct-equal)))
        (when (and (atom obj-a) (atom obj-b))
          (equal obj-a obj-b)))))

(defun empty-struct-p (obj)
  "Return non nil if the obj is (:obj t)."
  (and (consp obj)
       (= 2 (length obj))
       (handler-case (getf obj :obj)
         (t () nil))))

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

(defun build-wrapper-body (slot-descriptors node result-name)
  (with-gensyms (child matcher)
    (multiple-value-bind (clusters leaves)
        (cluster-by-selector-head slot-descriptors)
      (let ((clauses 
             (unless (zerop (hash-table-count clusters))
               `((let ,(loop 
                          for head being the hash-keys of clusters
                          for id from 0
                          collect `(,(symbolicate matcher "-" id) 
                                     (make-pattern-matcher ,head)))
                   (loop for ,child in (get-children ,node)
                      do ,@(loop 
                              for sub-descriptors being 
                              the hash-values of clusters
                              for id from 0
                              collect `(when (funcall 
                                              ,(symbolicate matcher "-" id)
                                              ,child)
                                         ,@(build-wrapper-body
                                            sub-descriptors
                                            child
                                            result-name)))))))))
        (when leaves
          (loop for descriptor in leaves
             do (push `(setf (getf ,result-name
                                   ,(get-slot-name descriptor))
                             (funcall ,(get-callback descriptor)
                                      ,node))
                      clauses)))
        clauses))))

(defun build-wrapper-lambda (slot-descriptors)
  (with-gensyms (node result)
    `(lambda (,node)
       (let ((,result '(:obj t)))
         ,@(build-wrapper-body (mapcar #'pre-expand-descriptor
                                       slot-descriptors)
                               node
                               result)
	 ,result))))

(defmacro def-struct-wrapper (name &body slot-descriptors)
  (with-gensyms (result node)
    `(defun ,name (,node)
       (let ((,result '(:obj t)))
         ,@(build-wrapper-body (mapcar #'pre-expand-descriptor
                                       slot-descriptors)
                               node
                               result)
         ,result))))

(defmacro make-struct-wrapper (&rest slot-descriptors)
  (build-wrapper-lambda slot-descriptors))


(defun pre-expand-callback (callback-clause)
  "Expand callback-clauses to be lambda clauses if they represent a
  call to macro make-struct-wrapper or make-list-wrapper."
  (if (consp callback-clause)
      (cond ((eq (car callback-clause) 'make-struct-wrapper)
             (build-wrapper-lambda (cdr callback-clause)))
            ((eq (car callback-clause) 'make-list-wrapper)
             (apply #'build-list-wrapper-lambda (cdr callback-clause)))
            (t callback-clause))
      callback-clause))

(defun pre-expand-descriptor (descriptor)
  "(Maybe) expand the callback part of the descriptor."
  (list (car descriptor)
        (cadr descriptor)
        (pre-expand-callback (caddr descriptor))))

(defun build-list-wrapper-body (selector callback node result-name)
  (with-gensyms (child matcher)
    (multiple-value-bind (head tail)
	(split-selector selector)
      (if (equal head "")
          `(push (funcall ,callback ,node)
                 ,result-name)
          `(let ((,matcher (make-pattern-matcher ,head)))
             (loop for ,child in (get-children ,node)
                do (when (funcall ,matcher ,child)
                     ,(build-list-wrapper-body
                       tail
                       callback
                       child
                       result-name))))))))

(defun build-list-wrapper-lambda (selector callback)
  (with-gensyms (result node)
    `(lambda (,node)
       (let ((,result nil))
         ,(build-list-wrapper-body selector 
                                   (pre-expand-callback callback)
                                   node result)
         ,result))))

(defmacro def-list-wrapper (name selector callback)
  (with-gensyms (result node)
    `(defun ,name (,node)
       (let ((,result nil))
         ,(build-list-wrapper-body selector 
                                   (pre-expand-callback callback) 
                                   node result)
         ,result))))

(defmacro make-list-wrapper (selector callback)
  (build-list-wrapper-lambda selector callback))


















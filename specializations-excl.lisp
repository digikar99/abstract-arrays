(cl:in-package :abstract-arrays)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defvar *element-type-p-fn-name-element-type-alist* nil)
  (defvar *rank-p-fn-name-rank-alist* nil)

  (defun element-type-p-fn-name (element-type)
    (let* ((element-type (introspect-environment:typexpand element-type))
           (fn-name (intern (uiop:strcat "ABSTRACT-ARRAY-ELEMENT-TYPE-"
                                         (write-to-string element-type)
                                         "-P")
                            (find-package :abstract-arrays))))
      (pushnew (cons fn-name element-type)
               *element-type-p-fn-name-element-type-alist*
               :key #'car)
      fn-name))

  (defun rank-p-fn-name (rank)
    (let ((fn-name (intern (uiop:strcat "ABSTRACT-ARRAY-RANK-"
                                        (write-to-string rank)
                                        "-P")
                           (find-package :abstract-arrays))))
      (pushnew (cons fn-name rank)
               *rank-p-fn-name-rank-alist*
               :key #'car)
      fn-name)))

(defmacro define-array-element-type-specialization (element-type)
  (let* ((fn-name      (element-type-p-fn-name element-type)))
    `(progn
       (declaim (inline ,fn-name))
       ,(if (eq 'cl:* element-type)
            `(defun ,fn-name (object)
               (typep object 'abstract-array))
            `(defun ,fn-name (object)
               (and (typep object 'abstract-array)
                    (alexandria:type= ',element-type
                                      (array-element-type (the abstract-array object)))))))))

(defmacro define-array-rank-specialization (rank)
  (check-type rank (or (eql cl:*) (integer 0 #.array-rank-limit)))
  (let ((fn-name (rank-p-fn-name rank)))
    `(progn
       (declaim (inline ,fn-name))
       ,(if (eq 'cl:* rank)
            `(defun ,fn-name (object)
               (typep object 'abstract-array))
            `(defun ,fn-name (object)
               (and (typep object 'abstract-array)
                    (= ,rank
                       (array-rank (the abstract-array object)))))))))

(defmacro define-array-specialization-type (type &optional (base-type 'abstract-array))
    "Defines a (TYPE &OPTIONAL ELEMENT-TYPE RANK) type for each RANK and ELEMENT-TYPE
using EXTENSIBLE-COMPOUND-TYPES:DEFINE-COMPOUND-TYPE.

Also defines a CL alias using SATISFIES type.
Additionally, defines POLYMORPHIC-FUNCTIONS:PARAMETRIC-TYPE-RUN-TIME-LAMBDA-BODY
and POLYMORPHIC-FUNCTIONS:PARAMETRIC-TYPE-COMPILE-TIME-LAMBDA-BODY corresponding
to TYPE.

BASE-TYPE should be the name of the CLASS upon which the arrays will be based.

See DENSE-ARRAYS:ARRAY for an example."
  `(progn

     (define-methods-for-parametric-type-lambda-bodies ,type)

     (define-compound-type ,type (o
                                  &optional (element-type 'cl:*)
                                  (dim/rank 'cl:*))
       ,(format nil
                "A wrapper around ~S with support for specifying ELEMENT-TYPE and DIMENSIONS or RANK.
These specializers are the same like the CL:ARRAY compound type."
                base-type)
       (and (cl:typep o ',base-type)
            (locally (declare (type ,base-type o))
              (or (eq 'cl:* element-type)
                  (type= element-type
                         (array-element-type o))))
            (locally (declare (type ,base-type o))
              (or (eq 'cl:* dim/rank)
                  (if (atom dim/rank)
                      (= (the fixnum dim/rank) (array-rank o))
                      (loop :for d1 :in dim/rank
                            :for d2 :in (array-dimensions o)
                            :always (or (eq 'cl:* d1)
                                        (= (the fixnum d1)
                                           (the fixnum d2)))))))))

     (defmethod %subtypep ((n1 (eql ',type)) (n2 (eql 'sequence)) t1 t2 &optional env)
       (declare (ignore n1 n2 t1 t2 env))
       (values nil t))
     (defmethod %subtypep ((n1 (eql 'sequence)) (n2 (eql ',type)) t1 t2 &optional env)
       (declare (ignore n1 n2 t1 t2 env))
       (values nil t))
     (defmethod %intersect-type-p ((n1 (eql ',type)) (n2 (eql 'sequence)) t1 t2 &optional env)
       (declare (ignore n1 n2 t1 t2 env))
       (values t t))
     (defmethod %intersect-type-p ((n1 (eql 'sequence)) (n2 (eql ',type)) t1 t2 &optional env)
       (declare (ignore n1 n2 t1 t2 env))
       (values t t))

     (defmethod %subtypep ((n1 (eql ',type)) (n2 (eql 'abstract-array)) t1 t2 &optional env)
       (declare (ignore n1 n2 t1 t2 env))
       (values t t))

     (defmethod %intersect-type-p
         ((t1 (eql ',type)) t2 type1 type2 &optional env)
       (declare (ignore t1 type1))
       ;; T2 is guaranteed to be type-expanded and be a symbol
       (if (and (or (symbolp type2)
                    (and (listp type2)
                         (car type2)
                         (null (cdr type2))))
                (find-class t2 nil env))
           (multiple-value-bind (nullp knownp)
               (cl:subtypep `(and ,',base-type ,t2) nil env)
             (values (not nullp) knownp))
           (call-next-method)))
     (defmethod %intersect-type-p
         (t1 (t2 (eql ',type)) type1 type2 &optional env)
       (%intersect-type-p t2 t1 type2 type1 env))

     (defmethod %subtypep
         ((t1 (eql ',type)) t2 type1 type2 &optional env)
       (declare (ignore t1 type1))
       ;; T2 is guaranteed to be type-expanded and be a symbol
       (if (and (or (symbolp type2)
                    (and (listp type2)
                         (car type2)
                         (null (cdr type2))))
                (find-class t2 nil env))
           (multiple-value-bind (subtypep knownp)
               (cl:subtypep ',base-type t2 env)
             (values subtypep knownp))
           (call-next-method)))
     (defmethod %subtypep
         (t1 (t2 (eql ',type)) type1 type2 &optional env)
       (declare (ignore t2 type2))
       ;; T2 is guaranteed to be type-expanded and be a symbol
       (if (and (or (symbolp type1)
                    (and (listp type1)
                         (car type1)
                         (null (cdr type1))))
                (find-class t1 nil env))
           (multiple-value-bind (subtypep knownp)
               (cl:subtypep t1 ',base-type env)
             (values subtypep knownp))
           (call-next-method)))

     (defmethod %upgraded-cl-type ((name (eql ',type)) type &optional env)
       (declare (ignore env))
       (destructuring-bind (&optional (element-type 'cl:* elt-supplied-p) (rank 'cl:* rankp))
           (rest (alexandria:ensure-list type))
         (when (listp rank) (setq rank (length rank)))
         (check-type rank (or (eql *) (integer 0 #.array-rank-limit)))
         (when (eq '* element-type) (setq elt-supplied-p nil))
         (when (eq '* rank)         (setq rankp nil))
         (let ((*package* (find-package :abstract-arrays)))
           (cond ((and rankp elt-supplied-p)
                  `(and ,',base-type
                        (satisfies ,(element-type-p-fn-name element-type))
                        (satisfies ,(rank-p-fn-name rank))))
                 (elt-supplied-p
                  `(and ,',base-type
                        (satisfies ,(element-type-p-fn-name element-type))))
                 (rankp                 ; never invoked though
                  `(and ,',base-type
                        (satisfies ,(rank-p-fn-name rank))))
                 (t
                  ',base-type)))))

     (defmethod %subtypep ((t1 (eql ',type)) (t2 (eql ',type)) type1 type2 &optional env)
       (declare (ignore t1 t2))
       (subtypep `(cl:array ,@(rest (alexandria:ensure-list type1)))
                 `(cl:array ,@(rest (alexandria:ensure-list type2)))
                 env))

     (defmethod %intersect-type-p
         ((t1 (eql ',type)) (t2 (eql ',type)) type1 type2 &optional env)
       (declare (ignore t1 t2))
       (intersect-type-p `(cl:array ,@(rest (alexandria:ensure-list type1)))
                         `(cl:array ,@(rest (alexandria:ensure-list type2)))
                         env))))

(defun array-type-element-type (array-type &optional env)
  "Similar to SANDALPHON.COMPILER-MACRO:ARRAY-TYPE-ELEMENT-TYPE; returns the
actual ELEMENT-TYPE corresponding to ARRAY-RANK in ENV.
ARRAY-TYPE is expected to be a subtype of ABSTRACT-ARRAY.
See also: DEFINE-ARRAY-SPECIALIZATIONS and DEFINE-ARRAY-SPECIALIZATION-TYPE"
  (let ((array-type (extensible-compound-types:typexpand array-type env)))
    (assert (subtypep array-type 'abstract-array)
            ()
            "Expected ARRAY-TYPE to be a SUBTYPE of ABSTRACT-ARRAYS:ABSTRACT-ARRAY but is~%  ~S"
            array-type)
    (optima:match array-type
      ((list* _ element-type _)
       element-type)
      ((variable _)
       'cl:*))))

(defun array-type-rank (array-type &optional env)
  "Similar to SANDALPHON.COMPILER-MACRO:ARRAY-TYPE-RANK; returns the actual RANK
corresponding to ARRAY-RANK in ENV.
ARRAY-TYPE is expected to be a subtype of ABSTRACT-ARRAY.
See also: DEFINE-ARRAY-SPECIALIZATIONS and DEFINE-ARRAY-SPECIALIZATION-TYPE"
  (let ((array-type (extensible-compound-types:typexpand array-type env)))
    (assert (subtypep array-type 'abstract-array)
            ()
            "Expected ARRAY-TYPE to be a SUBTYPE of ABSTRACT-ARRAYS:ABSTRACT-ARRAY but is~%  ~S"
            array-type)
    (optima:match array-type
      ((list _ _)
       'cl:*)
      ((list _ _ rank/dimensions)
       (typecase rank/dimensions
         (list (length rank/dimensions))
         (number rank/dimensions)
         (t 'cl:*)))
      ((variable _)
       'cl:*))))

(defun array-type-dimensions (array-type &optional env)
  "Similar to SANDALPHON.COMPILER-MACRO:ARRAY-TYPE-DIMENSIONS; returns the actual DIMENSIONS
corresponding to ARRAY-DIMENSIONS in ENV.
ARRAY-TYPE is expected to be a subtype of ABSTRACT-ARRAY.
See also: DEFINE-ARRAY-SPECIALIZATIONS and DEFINE-ARRAY-SPECIALIZATION-TYPE"
  (let ((array-type (extensible-compound-types:typexpand array-type env)))
    (assert (subtypep array-type 'abstract-array)
            ()
            "Expected ARRAY-TYPE to be a SUBTYPE of ABSTRACT-ARRAYS:ABSTRACT-ARRAY but is~%  ~S"
            array-type)
    (optima:match array-type
      ((symbol _)
       'cl:*)
      ((list _)
       'cl:*)
      ((list _ _)
       'cl:*)
      ((list _ _ rank/dimensions)
       (etypecase rank/dimensions
         (list rank/dimensions)
         (number (make-list rank/dimensions :initial-element 'cl:*))
         ((eql 'cl:*) 'cl:*))))))

;; For user consumption
(defmacro define-array-specializations ((&rest element-types) (&rest ranks))
  "Defines PREDICATES corresponding to each ELEMENT-TYPE and RANK (not their pairs).
The predicates of the two kinds will be independent of each other."
  `(progn
     ,@(loop :for e :in element-types
             :collect `(define-array-element-type-specialization ,e))
     ,@(loop :for rank :in ranks
             :collect `(define-array-rank-specialization ,rank))))

(define-array-specializations
    (single-float
     double-float
     (unsigned-byte 64)
     (unsigned-byte 32)
     (unsigned-byte 16)
     (unsigned-byte 08)
     (signed-byte 64)
     (signed-byte 32)
     (signed-byte 16)
     (signed-byte 08)
     nil
     bit
     fixnum
     t)
    #.(loop :for i :below 8 :collect i))

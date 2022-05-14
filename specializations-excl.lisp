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

BASE-TYPE should be the name of the CLASS upon which the arrays will be based.

Example, see DENSE-ARRAYS:ARRAY."
  `(progn

     (define-compound-type ,type (o
                                  &optional (element-type 'cl:* elt-supplied-p)
                                  (dim/rank 'cl:* dim/rankp))
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

     (defmethod %upgraded-cl-type ((name (eql ',type)) type &optional env)
       (destructuring-bind (&optional (element-type 'cl:* elt-supplied-p) (rank 'cl:* rankp))
           (rest (alexandria:ensure-list type))
         (when (listp rank) (setq rank (length rank)))
         (check-type rank (or (eql *) (integer 0 #.array-rank-limit)))
         (let ((*package* (find-package :abstract-arrays)))
           (cond ((and rankp elt-supplied-p)
                  `(and ,',base-type
                        (satisfies ,(element-type-p-fn-name element-type))
                        (satisfies ,(rank-p-fn-name rank))))
                 (elt-supplied-p
                  `(and ,',base-type
                        (satisfies ,(element-type-p-fn-name element-type))))
                 (rankp                   ; never invoked though
                  `(and ,',base-type
                        (satisfies ,(rank-p-fn-name rank))))
                 (t
                  ',base-type)))))

     (defmethod %subtypep ((t1 (eql ',type)) (t2 (eql ',type)) type1 type2 &optional env)
       (declare (ignore t1 t2 env))
       (labels ((dim-subtype-p (dim1 dim2)
                  (cond ((and (atom dim1) (atom dim2))
                         (or (eq dim2 'cl:*)
                             (and (not (eq dim1 'cl:*))
                                  (= dim1 dim2))))
                        ((and (atom dim2) (eq dim2 'cl:*))
                         t)
                        ((atom dim1)
                         nil)
                        ((= (length dim1) (length dim2))
                         (every #'dim-subtype-p dim1 dim2))
                        (t
                         nil))))
         (destructuring-bind (&optional (elt1 'cl:*) (dim1 'cl:*))
             (rest (alexandria:ensure-list type1))
           (destructuring-bind (&optional (elt2 'cl:*) (dim2 'cl:*))
               (rest (alexandria:ensure-list type2))
             (let ((dim-subtype-p (dim-subtype-p dim1 dim2)))
               (cond ((and (eq 'cl:* elt1) (eq 'cl:* elt2))
                      (values dim-subtype-p t))
                     ((eq 'cl:* elt1)
                      ;; TYPE1 is specific; TYPE2 is not
                      (values nil t))
                     ((eq 'cl:* elt2)
                      (values dim-subtype-p t))
                     ((type= (second type1) (second type2))
                      (values dim-subtype-p t))
                     (t
                      (values nil t))))))))))

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
      ((symbol _)
       'cl:*)
      ((list _)
       'cl:*)
      ((list* _ element-type _)
       element-type))))

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
      ((symbol _)
       'cl:*)
      ((list _)
       'cl:*)
      ((list _ _)
       'cl:*)
      ((list _ _ rank/dimensions)
       (typecase rank/dimensions
         (list (length rank/dimensions))
         (number rank/dimensions)
         (t 'cl:*))))))

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
     t
     cl:*)
    #.(cons 'cl:* (loop :for i :below 8 :collect i)))

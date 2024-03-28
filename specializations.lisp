(in-package :abstract-arrays)

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
using SATISFIES type.

BASE-TYPE should be the name of the CLASS upon which the arrays will be based.

See DENSE-ARRAYS:ARRAY for an example."
  `(cl:deftype ,type (&optional (element-type '* elt-supplied-p) (rank '* rankp))
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
             (rankp                   ; never invoked though
              `(and ,',base-type
                    (satisfies ,(rank-p-fn-name rank))))
             (t
              ',base-type)))))

(defun array-type-element-type (array-type &optional env)
    "Similar to SANDALPHON.COMPILER-MACRO:ARRAY-TYPE-ELEMENT-TYPE; returns the
actual ELEMENT-TYPE corresponding to ARRAY-RANK in ENV.
ARRAY-TYPE is expected to be a subtype of ABSTRACT-ARRAY.
See also: DEFINE-ARRAY-SPECIALIZATIONS and DEFINE-ARRAY-SPECIALIZATION-TYPE"
  (let ((array-type (peltadot:typexpand array-type env)))
    (assert (subtypep array-type 'abstract-array)
            ()
            "Expected ARRAY-TYPE to be a SUBTYPE of ABSTRACT-ARRAYS:ABSTRACT-ARRAY but is~%  ~S"
            array-type)
    (if (and (listp array-type)
             (eq 'and (first array-type)))
        (loop :for (element-type-p-fn-name . element-type) :in *element-type-p-fn-name-element-type-alist*
              :if (subtypep array-type `(satisfies ,element-type-p-fn-name))
                :do (return-from array-type-element-type element-type)
              :finally (return-from array-type-element-type 'cl:*))
        'cl:*)))

(defun array-type-rank (array-type &optional env)
  "Similar to SANDALPHON.COMPILER-MACRO:ARRAY-TYPE-RANK; returns the actual RANK
corresponding to ARRAY-RANK in ENV.
ARRAY-TYPE is expected to be a subtype of ABSTRACT-ARRAY.
See also: DEFINE-ARRAY-SPECIALIZATIONS and DEFINE-ARRAY-SPECIALIZATION-TYPE"
  (let ((array-type (peltadot:typexpand array-type env)))
    (assert (subtypep array-type 'abstract-array)
            ()
            "Expected ARRAY-TYPE to be a SUBTYPE of ABSTRACT-ARRAYS:ABSTRACT-ARRAY but is~%  ~S"
            array-type)
    (if (and (listp array-type)
             (eq 'and (first array-type)))
        (loop :for (rank-p-fn-name . rank) :in *rank-p-fn-name-rank-alist*
              :if (subtypep array-type `(satisfies ,rank-p-fn-name))
                :do (return-from array-type-rank rank)
              :finally (return-from array-type-rank 'cl:*))
        'cl:*)))

(defun array-type-dimensions (array-type &optional env)
  "Similar to SANDALPHON.COMPILER-MACRO:ARRAY-TYPE-DIMENSIONS; returns the actual DIMENSIONS
corresponding to ARRAY-DIMENSIONS in ENV.
ARRAY-TYPE is expected to be a subtype of ABSTRACT-ARRAY.
See also: DEFINE-ARRAY-SPECIALIZATIONS and DEFINE-ARRAY-SPECIALIZATION-TYPE"
  (declare (ignore array-type env))
  (error "ARRAY-TYPE-DIMENSIONS requires ABSTRACT-ARRAYS to be compiled with
:EXTENSIBLE-COMPOUND-TYPES in CL:*FEATURES*"))

;; For user consumption
(defmacro define-array-specializations ((&rest element-types) (&rest ranks))
  "Defines PREDICATES corresponding to each ELEMENT-TYPE and RANK (not their pairs).
The predicates of the two kinds will be independent of each other."
  `(progn
     ,@(loop :for e :in element-types
             :collect `(define-array-element-type-specialization ,e))
     ,@(loop :for rank :in ranks
             :collect `(define-array-rank-specialization ,rank))))

;; Some preprovided specializations
(define-array-specializations
    ((complex single-float)
     (complex double-float)
     single-float
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

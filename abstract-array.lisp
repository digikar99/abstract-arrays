(in-package :abstract-arrays)

(defmacro define-ordered-class-with-required-slots (name direct-superclasses direct-slots
                                                    &rest slot-options)
  "Like DEFINE-ORDERED-CLASS but slots can also have a `:required t` as an option.
This is substituted with a error-ing :initform."
  `(define-ordered-class ,name ,direct-superclasses
     ,(loop :for direct-slot :in direct-slots
            :collect
            (progn
              (when (getf (cdr direct-slot) :required)
                (setf (getf (cdr direct-slot) :initform)
                      `(cl:error
                        ,(format nil
                                 "~S must be supplied during ~S initialization"
                                 (first direct-slot)
                                 name))))
              (remf (cdr direct-slot) :required)
              (let ((slot-name (symbol-name (first direct-slot))))
                (append direct-slot `(:initarg
                                      ,(intern slot-name
                                               (find-package :keyword))
                                      :inline t)
                        (if (getf (rest direct-slot) :reader)
                            ()
                            `(:reader ,(intern (concatenate 'string
                                                            (symbol-name name)
                                                            "-"
                                                            slot-name)
                                               (symbol-package name))))))))
     ,@slot-options))

(macrolet ((def (name return-type)
             `(progn
                (define-polymorphic-function ,name (array))
                (defpolymorph ,name ((array cl:array)) ,return-type
                  (,(find-symbol (symbol-name name) :cl) array)))))
  (def array-dimensions   list)
  (def array-rank         (integer 0 #.array-rank-limit))
  (def array-element-type t)
  (def array-total-size   (integer 0 #.array-total-size-limit)))

(define-polymorphic-function array-storage (array) :overwrite t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (alexandria:define-constant +abstract-array-slot-order+
      '(storage dimensions element-type rank total-size)
    :test #'equal))

(define-ordered-class-with-required-slots abstract-array ()
  ((storage      :required t
                 :polymorph t :reader array-storage)
   ;; This list is not expected to be modified; therefore, we do a bit unusual thing
   ;; to bring attention of the user
   (dimensions   :required t :type list
                 :polymorph nil :reader abstract-array-dimensions)
   (element-type :required t
                 :polymorph t :reader array-element-type)
   (rank         :required t :type (integer 0 #.array-rank-limit)
                 :polymorph t :reader array-rank)
   (total-size   :required t :type (integer 0 #.array-total-size-limit)
                 :polymorph t :reader array-total-size))
  (:metaclass abstract-array-class)
  (:order #.+abstract-array-slot-order+))

(setf (documentation 'abstract-array-dimensions 'function)
      "Access the DIMENSIONS list of the ABSTRACT-ARRAY. Destructively modifying
this list would result in a change in the DIMENSIONS of the array; hence use
this only for read-only access to the DIMENSIONS.")

;; FIXME: This does not work with displaced arrays
(defpolymorph array-storage ((array cl:array)) cl:vector
  (declare (ignorable array))
  #+sbcl (sb-ext:array-storage-vector array)
  #-sbcl (error "ARRAY-STORAGE not implemented for CL:ARRAY!"))

#+sbcl
(defmethod slot-unbound (class
                         (instance abstract-array-class)
                         (slot-name (eql 'sb-pcl::%class-precedence-list)))
  ;; FIXME: Is this correct?
  (setf (slot-value instance slot-name)
        (mapcar #'find-class '(abstract-array t))))

(defmacro define-array-class (name &body (direct-slots . slot-options))
  "Defines NAME as a CLASS with DIRECT-SUPERCLASS ABSTRACT-ARRAY and metaclass
as ABSTRACT-ARRAY-CLASS. Also defines the appropriate order using DIRECT-SLOTS."
  `(define-ordered-class-with-required-slots ,name (abstract-array)
     ,direct-slots
     (:order ,(append +abstract-array-slot-order+ (mapcar #'first direct-slots)))
     ,@slot-options))

(in-package :abstract-arrays)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun element-type-p-fn-name (element-type)
    (intern (uiop:strcat "ABSTRACT-ARRAY-ELEMENT-TYPE-"
                         (write-to-string (introspect-environment:typexpand element-type))
                         "-P")
            (find-package :abstract-arrays)))

  (defun rank-p-fn-name (rank)
    (intern (uiop:strcat "ABSTRACT-ARRAY-RANK-"
                         (write-to-string rank)
                         "-P")
            (find-package :abstract-arrays)))

  (defun intersection-type-types (type &optional env)
    (let ((type (introspect-environment:typexpand type env)))
      (if (and (listp type) (eq 'and (first type)))
          (loop :for type :in (rest type)
                :appending (intersection-type-types type env))
          (list type)))))

(defmacro define-array-element-type-specialization (element-type)
  (let* ((fn-name      (element-type-p-fn-name element-type)))
    (if (eq 'cl:* element-type)
        `(defun ,fn-name (object)
           (typep object 'abstract-array))
        `(defun ,fn-name (object)
           (and (typep object 'abstract-array)
                (alexandria:type= ',element-type
                                  (array-element-type object)))))))

(defmacro define-array-rank-specialization (rank)
  (check-type rank (or (eql cl:*) (integer 0 #.array-rank-limit)))
  (let ((fn-name (rank-p-fn-name rank)))
    `(defun ,fn-name (object)
       (and (typep object 'abstract-array)
            (= ,rank (array-rank object))))))

(defmacro define-array-specialization-type (type &optional (base-type 'abstract-array))
    "Defines a (TYPE &OPTIONAL ELEMENT-TYPE RANK) type for each RANK and ELEMENT-TYPE
using SATISFIES type."
  `(deftype ,type (&optional (element-type '* elt-supplied-p) (rank '* rankp))
     (check-type rank (or (eql *) (integer 0 #.array-rank-limit)))
     (let ((*package* (find-package :abstract-arrays)))
       (cond ((and rankp elt-supplied-p)
              `(and ,',base-type
                    (satisfies ,(element-type-p-fn-name element-type))
                    (satisfies ,(rank-p-fn-name rank))))
             (elt-supplied-p
              `(and ,',base-type
                    (satisfies ,(element-type-p-fn-name element-type))))
             (rankp                       ; never invoked though
              `(and ,',base-type
                    (satisfies ,(rank-p-fn-name rank))))
             (t
              ',base-type)))))

(defun array-type-element-type (array-type &optional env)
  (let ((array-type (introspect-environment:typexpand array-type env)))
    (assert (subtypep array-type 'abstract-array)
            ()
            "Expected ARRAY-TYPE to be a SUBTYPE of ABSTRACT-ARRAYS:ABSTRACT-ARRAY but is~%  ~S"
            array-type)
    (if (and (listp array-type)
             (eq 'and (first array-type)))
        (loop :for clause :in (intersection-type-types array-type)
              :if (and (listp clause)
                       (eq 'satisfies (first clause)))
                :do (let* ((fn-name (symbol-name (second clause)))
                           (prefix  "ABSTRACT-ARRAY-ELEMENT-TYPE-")
                           (elt-pos (search prefix fn-name))
                           (start-pos (length prefix))
                           (end-pos (search "-P" fn-name)))
                      (when (and elt-pos end-pos)
                        (return-from array-type-element-type
                          (let ((*read-eval* nil))
                            (read-from-string (subseq fn-name start-pos end-pos))))))
              :finally (return-from array-type-element-type 'cl:*))
        'cl:*)))

(defun array-type-rank (array-type &optional env)
  (let ((array-type (introspect-environment:typexpand array-type env)))
    (assert (subtypep array-type 'abstract-array)
            ()
            "Expected ARRAY-TYPE to be a SUBTYPE of ABSTRACT-ARRAYS:ABSTRACT-ARRAY but is~%  ~S"
            array-type)
    (if (and (listp array-type)
             (eq 'and (first array-type)))
        (loop :for clause :in (intersection-type-types array-type)
              :if (and (listp clause)
                       (eq 'satisfies (first clause)))
                :do (let* ((fn-name (symbol-name (second clause)))
                           (prefix  "ABSTRACT-ARRAY-RANK-")
                           (elt-pos (search prefix fn-name))
                           (start-pos (length prefix))
                           (end-pos (search "-P" fn-name)))
                      (when (and elt-pos end-pos)
                        (return-from array-type-rank
                          (let ((*read-eval* nil))
                            (read-from-string (subseq fn-name start-pos end-pos))))))
              :finally (return-from array-type-rank 'cl:*))
        'cl:*)))

;; For user consumption
(defmacro define-array-specializations ((&rest element-types) (&rest ranks))
  `(progn
     ,@(loop :for e :in element-types
             :collect `(define-array-element-type-specialization ,e))
     ,@(loop :for rank :in ranks
             :collect `(define-array-rank-specialization ,rank))))

;; Some preprovided specializations
(ignore-errors
 (define-array-specializations
     (single-float
      double-float
      (unsigned-byte 64)
      (unsigned-byte 32)
      (unsigned-byte 08)
      (signed-byte 64)
      (signed-byte 32)
      bit
      t
      cl:*)
     #.(cons 'cl:* (loop :for i :below 8 :collect i))))

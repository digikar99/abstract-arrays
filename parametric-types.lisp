(in-package :abstract-arrays)

(defun parametric-type-run-time-lambda-body-for-arrays (type-car type-cdr parameter)
  `(cl:lambda (array)
     (declare (compiler-macro-notes:muffle
               compiler-macro-notes:optimization-failure-note))
     (when (typep array ',type-car)
       (locally (declare (type ,type-car array))
         ,(optima:match type-cdr
            ((list* (eql parameter) _)
             `(array-element-type array))
            ((list _ (eql parameter))
             `(array-rank array))
            ((optima:guard (list _ dimensions)
                           (position parameter dimensions))
             `(array-dimension array ,(position parameter dimensions)))
            (otherwise
             (error "TYPE-PARAMETER ~S not in PARAMETRIC-TYPE ~S"
                    parameter (cons type-car type-cdr))))))))

(defun parametric-type-compile-time-lambda-body-for-arrays (type-car type-cdr parameter)
  `(cl:lambda (type)
     (declare (ignorable type))
     (optima:match type
       ((optima:guard (list* ft-type-car _)
                      (and ft-type-car
                           (subtypep ft-type-car ',type-car)))
        ,(optima:match type-cdr
           ((list* (eql parameter) _)
            `(let ((elt-type (array-type-element-type type)))
               (if (eq elt-type 'cl:*)
                   nil
                   elt-type)))
           ((list _ (eql parameter))
            `(let ((rank (array-type-rank type)))
               (if (eq rank 'cl:*)
                   nil
                   rank)))
           ((optima:guard (list _ dimensions)
                          (position parameter dimensions))
            (let ((pos (position parameter dimensions)))
              `(let ((dimension (nth ,pos (array-type-dimensions type))))
                 (if (eq dimension 'cl:*)
                     nil
                     dimension))))
           (otherwise
            (error "TYPE-PARAMETER ~S not in PARAMETRIC-TYPE ~S"
                   parameter (cons type-car type-cdr)))))
       (otherwise nil))))

(defmacro define-methods-for-parametric-type-lambda-bodies (type)
  `(progn
     (defmethod polymorphic-functions:parametric-type-run-time-lambda-body
         ((type-car (eql ',type)) type-cdr parameter)
       (parametric-type-run-time-lambda-body-for-arrays type-car type-cdr parameter))
     (defmethod polymorphic-functions:parametric-type-compile-time-lambda-body
         ((type-car (eql ',type)) type-cdr parameter)
       (parametric-type-compile-time-lambda-body-for-arrays type-car type-cdr parameter))))

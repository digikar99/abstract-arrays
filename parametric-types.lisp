(in-package :abstract-arrays)

(defvar *array-class-type-alist* ()
  "An ALIST used by POLYMORPHIC-FUNCTIONS:SPECIALIZING-TYPE-OF to return the
appropriate type associated with the array class. The CAR of each entry of
the alist should be the class or class-name of the concerned array object,
while the CDR should be a symbol or a function. If the CDR is a symbol,
then it forms the CAR of the returned type-specifier; if it is a function,
then the function should take the array-object as the input and return
a symbol to be used as the CAR of the type-specifier.

See dense-arrays/src/types.lisp for an instance of such a function.")

(polymorphic-functions:defpolymorph polymorphic-functions:specializing-type-of
    ((object abstract-arrays:abstract-array))
    (or symbol cons)
  (let* ((class      (class-of object))
         (class-name (class-name class))
         (type-or-function (or (alexandria:assoc-value *array-class-type-alist* class-name)
                               (alexandria:assoc-value *array-class-type-alist* class))))
    (etypecase type-or-function
      (function (list (funcall type-or-function object)
                      (array-element-type object)
                      (array-rank object)))
      (null class-name)
      (symbol (list type-or-function (array-element-type object) (array-rank object))))))

(defun atom-like-p (atom-or-list)
  (or (atom atom-or-list)
      (and (listp atom-or-list)
           (first atom-or-list)
           (null (rest atom-or-list)))))

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
            (_
             (error "TYPE-PARAMETER ~S not in PARAMETRIC-TYPE ~S"
                    parameter (cons type-car type-cdr))))))))

(defun parametric-type-compile-time-lambda-body-for-arrays (type-car type-cdr parameter)
  `(cl:lambda (type)
     (declare (ignorable type))
     ,(optima:match type-cdr
        ((list* (eql parameter) _)
         `(let ((elt-type (array-type-element-type type)))
            (if (eq elt-type 'cl:*)
                nil
                (values elt-type t))))
        ((list _ (eql parameter))
         `(let ((rank (array-type-rank type)))
            (if (eq rank 'cl:*)
                nil
                (values rank t))))
        ((optima:guard (list _ dimensions)
                       (position parameter dimensions))
         (let ((pos (position parameter dimensions)))
           `(let ((dimension (nth ,pos (array-type-dimensions type))))
              (if (eq dimension 'cl:*)
                  nil
                  (values dimension t)))))
        (_
         (error "TYPE-PARAMETER ~S not in PARAMETRIC-TYPE ~S"
                parameter (cons type-car type-cdr))))))

(defmacro define-methods-for-parametric-type-lambda-bodies (type)
  `(progn
     (defmethod polymorphic-functions:parametric-type-run-time-lambda-body
         ((type-car (eql ',type)) type-cdr parameter)
       (parametric-type-run-time-lambda-body-for-arrays type-car type-cdr parameter))
     (defmethod polymorphic-functions:parametric-type-compile-time-lambda-body
         ((type-car (eql ',type)) type-cdr parameter)
       (parametric-type-compile-time-lambda-body-for-arrays type-car type-cdr parameter))))

(in-package :abstract-arrays)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *array-class-type-alist* ()
    "An ALIST used by PELTADOT:SPECIALIZING-TYPE-OF to return the
appropriate type associated with the array class. The CAR of each entry of
the alist should be the class or class-name of the concerned array object,
while the CDR should be a symbol or a function. If the CDR is a symbol,
then it forms the CAR of the returned type-specifier; if it is a function,
then the function should take the array-object as the input and return
a symbol to be used as the CAR of the type-specifier.

See dense-arrays/src/types.lisp for an instance of such a function."))

(defpolymorph specializing-type-of
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

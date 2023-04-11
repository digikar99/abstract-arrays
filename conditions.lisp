(in-package :abstract-arrays)

(define-condition invalid-array-index (error)
  ((index :initarg :index)
   (array :initarg :array)
   (suggestion :initarg :suggestion :initform nil))
  (:report (lambda (condition stream)
             (let ((*print-array* nil))
               (with-slots (index array suggestion) condition
                 (format stream "Index ~S is invalid for array~%  ~S~%of dimensions ~D"
                         index array (narray-dimensions array))
                 (when suggestion
                   (format stream "~%~%~A" suggestion)))))))

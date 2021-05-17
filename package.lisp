(cl:in-package #:abstract-arrays/defpackage)
#.(cl:let ((export-symbols '(#:abstract-array
                             #:abstract-array-class

                             #:array-storage
                             #:array-storage-ref
                             #:array-storage-set

                             #:array-dimensions
                             #:array-dimension
                             #:array-rank
                             #:array-element-type
                             #:array-total-size
                             #:arrayp

                             #:aref
                             #:row-major-aref

                             #:define-array-specializations
                             #:define-array-specialization-type
                             #:array-type-element-type
                             #:array-type-rank)))

    `(uiop:define-package :abstract-arrays
         (:mix :cl :adhoc-polymorphic-functions)
       (:shadow ,@export-symbols)
       (:export ,@export-symbols)))

(in-package #:abstract-arrays)

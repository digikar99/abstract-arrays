(cl:in-package #:cl-user)

(defpackage #:abstract-arrays/defpackage
  (:use :cl)
  (:intern #:export-symbols))

(in-package #:abstract-arrays/defpackage)

(defun export-symbols ()
  '(#:abstract-array
    #:abstract-array-class

    #:array-storage
    #:array-storage-ref
    #:array-storage-set

    #:array-dimensions
    #:narray-dimensions
    #:abstract-array-dimensions
    #:array-dimension
    #:array-rank
    #:array-element-type
    #:array-total-size
    #:arrayp

    #:aref
    #:row-major-aref

    #:define-array-class
    #:define-array-specializations
    #:define-array-specialization-type
    #:array-type-element-type
    #:array-type-rank
    #:array-type-dimensions))

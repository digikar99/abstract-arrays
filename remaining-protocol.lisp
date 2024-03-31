(in-package :abstract-arrays)

(define-polymorphic-function narray-dimensions (array))
(declaim (cl:ftype (function * list) narray-dimensions))
(defpolymorph narray-dimensions ((array abstract-array)) list
  "Returns the dimensions of the ARRAY. The consequences are undefined if the
returned dimensions are modified. Use ARRAY-DIMENSIONS if destructive usage of
the returned list is intended."
  (abstract-array-dimensions array))

;;; Redefine and copy-list, because, we don't want users
;;; to assume destructive modification is okay
(defpolymorph array-dimensions ((array abstract-array)) list
  "Returns a COPY of the dimensions of ARRAY. The copy may then be modified.

See NARRAY-DIMENSIONS or equivalent of a copy is to be avoided, and destructive
use is not intended."
  (copy-list (abstract-array-dimensions array)))

(declaim (inline array-dimension))
(defun array-dimension (array axis-number)
  (nth axis-number (array-dimensions array)))

(declaim (inline arrayp))
(defun arrayp (object)
  (or (cl:arrayp object)
      (typep object 'abstract-array)))


;; FIXME: SBCL doesn't call the compiler-macro on APPLY, does anyone do it?
(define-polymorphic-function aref (array &rest subscripts) :overwrite t
  :documentation "This is SETF-able."
  :dispatch-declaration '(optimize speed))
(defpolymorph aref ((array cl:array) &rest subscripts) t
  #.(concatenate 'string
                 "A wrapper around CL:AREF."
                 (string #\newline)
                 (string #\newline)
                 (documentation 'cl:aref 'cl:function))
  (declare (dynamic-extent subscripts))
  (apply #'cl:aref array subscripts))

(define-polymorphic-function (setf aref) (new array &rest subscripts) :overwrite t)
(defpolymorph (setf aref) (new (array cl:array) &rest subscripts) t
  (declare (dynamic-extent subscripts))
  (setf (apply #'cl:aref array subscripts) new))

(define-polymorphic-function row-major-aref (array index) :overwrite t
  :documentation
  "Return the element of ARRAY corresponding to the row-major INDEX.
This is SETFable.")
(defpolymorph row-major-aref ((array cl:array) index) t
  (cl:row-major-aref array index))

(define-polymorphic-function (setf row-major-aref) (new array index) :overwrite t)
(defpolymorph (setf row-major-aref) (new (array cl:array) index) t
  (setf (cl:row-major-aref array index) new))

(define-polymorphic-function col-major-aref (array index) :overwrite t
  :documentation "Return the element of ARRAY corresponding to the column-major INDEX.
This is SETFable.")
(defpolymorph col-major-aref ((array cl:array) index) t
  (let ((col-major-index
          (loop :for d :in (cl:array-dimensions array)
                :with s := 1
                :with col-major-index := 0
                :do (incf col-major-index (* s (floor index s)))
                    (setf index (rem index s))
                    (setf s (* s d))
                :finally (return col-major-index)))
        (initial-offset (nth-value 1 (cl:array-displacement array))))
    (cl:aref (array-storage array)
             (+ initial-offset col-major-index))))
;; TODO: Implement a compiler-macro for this

(define-polymorphic-function (setf col-major-aref) (new array index) :overwrite t)
(defpolymorph (setf col-major-aref) (new (array cl:array) index) t
  (let ((col-major-index
          (loop :for d :in (cl:array-dimensions array)
                :with s := 1
                :with col-major-index := 0
                :do (incf col-major-index (* s (floor index s)))
                    (setf index (rem index s))
                    (setf s (* s d))
                :finally (return col-major-index)))
        (initial-offset (nth-value 1 (cl:array-displacement array))))
    (setf (cl:aref (array-storage array)
                   (+ initial-offset col-major-index))
          new)))

(declaim (inline column-major-aref (setf column-major-aref)))
(defun column-major-aref (array index)
  "Return the element of ARRAY corresponding to the column-major INDEX.
This is SETFable, and a wrapper around COL-MAJOR-AREF."
  (col-major-aref array index))
(define-compiler-macro column-major-aref (array-form index-form)
  `(col-major-aref ,array-form ,index-form))
(defun (setf column-major-aref) (new array index)
  (funcall #'(setf col-major-aref) new array index))
(define-compiler-macro (setf column-major-aref) (array-form index-form)
  `(funcall #'(setf col-major-aref) ,array-form ,index-form))

;;; TODO: Implement these for CLHS arrays
;;; FIXME: This isn't the fastest way of ref-ing. See dense-arrays backends.
(define-polymorphic-function array-storage-ref (array &rest subscripts))
(define-polymorphic-function array-storage-set (new-value array &rest subscripts))
(declaim (inline (setf array-storage-ref)))
(defun (setf array-storage-ref) (new-value array &rest subscripts)
  (apply #'array-storage-set new-value array subscripts))
(define-compiler-macro (setf array-storage-ref) (&whole form &rest args)
  (declare (ignore args))
  (if (eq 'funcall (first form))
      `(array-storage-set ,@(nthcdr 2 form))
      (print form)))

(define-trait-implementation traits:container abstract-array (t t)
  (defun traits:at (arr &rest idx)
    (apply #'aref arr idx))
  (define-compiler-macro traits:at (arr &rest idx)
    `(aref ,arr ,@idx))
  (defun (setf traits:at) (new-val arr &rest idx)
    (apply #'(setf aref) new-val arr idx))
  (define-compiler-macro (setf traits:at) (new-val arr &rest idx)
    `(funcall #'(setf aref) ,new-val ,arr ,@idx))
  (defun traits:size (arr)
    (array-total-size arr))
  (defun traits:capacity (arr)
    (array-total-size arr)))

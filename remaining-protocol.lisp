(in-package :abstract-arrays)

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
(define-polymorphic-function aref (array &rest subscripts) :overwrite t)
(defpolymorph aref ((array cl:array) &rest subscripts) t
  (declare (dynamic-extent subscripts))
  (apply #'cl:aref array subscripts))

(define-polymorphic-function (setf aref) (new array &rest subscripts) :overwrite t)
(defpolymorph (setf aref) (new (array cl:array) &rest subscripts) t
  (declare (dynamic-extent subscripts))
  (setf (apply #'cl:aref array subscripts) new))

(define-polymorphic-function row-major-aref (array index) :overwrite t
  :documentation
  "Return the element of ARRAY corresponding to the row-major INDEX.
This is SETFable")
(defpolymorph row-major-aref ((array cl:array) index) t
  (cl:row-major-aref array index))

(define-polymorphic-function (setf row-major-aref) (new array index) :overwrite t)
(defpolymorph (setf row-major-aref) (new (array cl:array) index) t
  (setf (cl:row-major-aref array index) new))


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

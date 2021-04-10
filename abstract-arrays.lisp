(cl:in-package :abstract-arrays)

;;; By providing these in the form of polymorphic-functions allows us to
;;; provide wrappers around the CLHS functions

(macrolet ((def (name return-type)
             `(progn
                (define-polymorphic-function ,name (array))
                (defpolymorph ,name ((array cl:array)) ,return-type
                  (,(find-symbol (symbol-name name) :cl) array))
                (defpolymorph ,name ((array abstract-array)) ,return-type
                  (,(find-symbol (uiop:strcat "ABSTRACT-" (symbol-name name))
                                 :abstract-arrays)
                   array)))))
  (def array-dimensions   list)
  (def array-rank         (integer 0 #.array-rank-limit))
  (def array-element-type t)
  (def array-total-size   (integer 0 #.array-total-size-limit)))

;;; Redefine and copy-list, because, we don't want users
;;; to assume destructive modification is okay
(defpolymorph array-dimensions ((array abstract-array)) list
  (copy-list (abstract-array-dimensions array)))



(declaim (inline array-dimension))
(defun array-dimension (array axis-number)
  (nth axis-number (array-dimensions array)))



(declaim (inline arrayp))
(defun arrayp (object)
  (or (cl:arrayp object)
      (abstract-array-p object)))

(declaim (inline array-storage))
(define-polymorphic-function array-storage (array) :overwrite t)
(defpolymorph array-storage ((abstract-array abstract-array)) t
  (declare (optimize speed))
  (abstract-array-storage abstract-array))



;; FIXME: SBCL doesn't call the compiler-macro on APPLY, does anyone do it?
(define-polymorphic-function aref (array &rest subscripts) :overwrite t)
(defpolymorph aref ((array cl:array) &rest subscripts) t
  (declare (dynamic-extent subscripts)
           (optimize speed))
  (apply #'cl:aref array subscripts))

(define-polymorphic-function (setf aref) (new array &rest subscripts) :overwrite t)
(defpolymorph (setf aref) (new (array cl:array) &rest subscripts) t
  (declare (dynamic-extent subscripts)
           (optimize speed))
  (setf (apply #'cl:aref array subscripts) new))


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

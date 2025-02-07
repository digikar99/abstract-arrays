(in-package :abstract-arrays)

(define-trait array () ()
  (array-dimensions (array) list)
  (array-dimension (array (integer 0 #.array-rank-limit))
                   (integer 0 #.array-dimension-limit))
  (array-rank (array) (integer 0 #.array-rank-limit))
  (array-element-type (array) (or cons symbol))
  (array-total-size (array) (integer 0 #.array-total-size-limit))
  (array-storage (array) t))

(define-trait-implementation array cl:array ()

  (defun array-dimensions (array) (cl:array-dimensions array))
  (defun array-dimension (array index) (cl:array-dimension array index))
  (defun array-rank (array) (cl:array-rank array))
  (defun array-element-type (array) (cl:array-element-type array))
  (defun array-total-size (array) (cl:array-total-size array))

  (defun array-storage (array)
    (declare (ignorable array)
             (optimize speed))
    ;; FIXME: This does not work with displaced arrays
    ;; Re: FIXME: It does work, right?
    #+sbcl (loop :with array := array
                 :do (locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
                       (typecase array
                         ((cl:simple-array * (*)) (return array))
                         (cl:simple-array (return (sb-ext:array-storage-vector array)))
                         (t (setq array (cl:array-displacement array))))))
    #+ccl (loop :with array := array
                :do (typecase array
                      ((cl:simple-array * (*)) (return array))
                      (cl:simple-array
                       (return (ccl::%array-header-data-and-offset array)))
                      (t (setq array (cl:array-displacement array)))))
    #-(or sbcl ccl)
    (error "ARRAY-STORAGE not implemented for CL:ARRAY on ~A!"
           (lisp-implementation-type))))

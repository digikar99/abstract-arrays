(in-package :abstract-arrays)

(defmacro defstruct* (name-and-options &body slot-descriptions)
  "Like DEFSTRUCT but slots can also have a `:required t` as an option.
This is substituted with a error-ing :initform."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (cl:defstruct ,name-and-options
       ,@(loop :for slot-description :in slot-descriptions
               :if (listp slot-description)
                 :collect
                 (cons (first slot-description)
                       (cond ((getf (rest slot-description) :initform)
                              (cons (getf (rest slot-description) :initform)
                                    (progn
                                      (remf (rest slot-description) :initform)
                                      (remf (rest slot-description) :required)
                                      (rest slot-description))))
                             ((getf (rest slot-description) :required)
                              (cons `(cl:error
                                      ,(format nil
                                               "~S must be supplied during ~S initialization"
                                               (first slot-description)
                                               (first name-and-options)))
                                    (progn
                                      (remf (rest slot-description) :required)
                                      (rest slot-description))))
                             (t
                              (cons nil (rest slot-description)))))
               :else
                 :collect slot-description))))


(defstruct* (abstract-array (:constructor nil))
  (storage      :required t :read-only t)
  ;; This list is not expected to be modified; therefore, we do a bit unusual thing
  ;; to bring attention of the user
  (dimensions   :required t :type list :read-only t)
  (element-type :required t :read-only t)
  (rank         :required t :type (integer 0 #.array-rank-limit) :read-only t)
  (total-size   :required t :type (integer 0 #.array-total-size-limit) :read-only t))
(define-orthogonally-specializing-type abstract-array () ())


(define-trait-implementation array abstract-array ()

;;; copy-list, because, we don't want users to assume destructive modification is okay
  (defun array-dimensions (array)
    "Returns a COPY of the dimensions of ARRAY. The copy may then be modified.

See NARRAY-DIMENSIONS or equivalent of a copy is to be avoided, and destructive
use is not intended."
    (copy-list (abstract-array-dimensions array)))
  (defun array-dimension (array index) (nth index (abstract-array-dimensions array)))
  (defun array-rank (array) (abstract-array-rank array))
  (defun array-element-type (array) (abstract-array-element-type array))
  (defun array-total-size (array) (abstract-array-total-size array))
  (defun array-storage (array) (abstract-array-storage array)))

(cl:in-package :cl)
#.(cl:let ((export-symbols '(abstract-array

                             array-storage
                             array-storage-ref
                             array-storage-set
                             
                             array-dimensions
                             array-dimension
                             array-rank
                             array-element-type
                             array-total-size
                             arrayp

                             aref
                             
                             define-array-specializations
                             define-array-specialization-type
                             array-type-element-type
                             array-type-rank)))
    `(uiop:define-package abstract-arrays
         (:mix cl adhoc-polymorphic-functions)
       (:shadow ,@export-symbols)
       (:export ,@export-symbols)))

(in-package abstract-arrays)

(defmacro define-struct-with-required-slots (name-and-options &rest slot-descriptions)
  "Like DEFSTRUCT but SLOT-DESCRIPTIONS can also have a `:required t` as an option."
  `(defstruct ,name-and-options
     ,(if (stringp (first slot-descriptions))
          (first slot-descriptions)
          "")
     ,@(loop :for desc :in (if (stringp (first slot-descriptions))
                               (rest slot-descriptions)
                               slot-descriptions)
             :collect (if (getf (cddr desc) :required)
                          `(,(first desc)
                            (cl:error ,(format nil
                                               "~S must be supplied during ~A:~A initialization"
                                               (first desc)
                                               (package-name
                                                (symbol-package
                                                 (first name-and-options)))
                                               (first name-and-options)))
                            ,@(progn (remf (cddr desc) :required)
                                     (cddr desc)))
                          desc))))

(define-struct-with-required-slots (abstract-array (:constructor nil)
                                                   (:copier nil))
  (storage      nil :required t)
  (dimensions   nil :required t :type list)
  (element-type t   :required t)
  (rank         nil :required t :type (integer 0 #.array-rank-limit))
  (total-size   nil :required t :type (integer 0 #.array-total-size-limit)))


(in-package :abstract-arrays)

(defclass abstract-array-class (standard-class) ())

(defmethod closer-mop:validate-superclass
    ((class abstract-array-class) (superclass standard-class))
  t)

(defmacro define-class-with-required-slots (name direct-superclasses direct-slots
                                            &rest slot-options)
  "Like DEFCLASS but slots can also have a `:required t` as an option.
This is substituted with a error-ing :initform."
  `(defclass ,name ,direct-superclasses
     ,(loop :for direct-slot :in direct-slots
            :collect
            (progn
              (when (getf (cdr direct-slot) :required)
                (setf (getf (cdr direct-slot) :initform)
                      `(cl:error
                        ,(format nil
                                 "~S must be supplied during ~S initialization"
                                 (first direct-slot)
                                 name))))
              (remf (cdr direct-slot) :required)
              (append direct-slot `(:initarg ,(intern (symbol-name (first direct-slot))
                                                      (find-package :keyword))))))
     ,@slot-options))

(define-class-with-required-slots abstract-array ()
  ((storage      :required t)
   (dimensions   :required t :type list)
   (element-type :required t)
   (rank         :required t :type (integer 0 #.array-rank-limit))
   (total-size   :required t :type (integer 0 #.array-total-size-limit)))
  (:metaclass abstract-array-class))

;; (define-class-with-required-slots abstract-array ()
;;   ((rank         :required t :type (integer 0 #.array-rank-limit)))
;;   (:metaclass abstract-array-class))

;; (defmethod initialize-instance :before ((object abstract-array) &key)
;;   (error "Illegal to initialize an ABSTRACT-ARRAY! Subclass ABSTRACT-ARRAY and then initialize!"))

;; (defmacro define-struct-with-required-slots (name-and-options &rest slot-descriptions)
;;   "Like DEFSTRUCT but SLOT-DESCRIPTIONS can also have a `:required t` as an option."
;;   `(defstruct ,name-and-options
;;      ,(if (stringp (first slot-descriptions))
;;           (first slot-descriptions)
;;           "")
;;      ,@(loop :for desc :in (if (stringp (first slot-descriptions))
;;                                (rest slot-descriptions)
;;                                slot-descriptions)
;;              :collect (if (getf (cddr desc) :required)
;;                           `(,(first desc)
;;                             (cl:error ,(format nil
;;                                                "~S must be supplied during ~A:~A initialization"
;;                                                (first desc)
;;                                                (package-name
;;                                                 (symbol-package
;;                                                  (first name-and-options)))
;;                                                (first name-and-options)))
;;                             ,@(progn (remf (cddr desc) :required)
;;                                      (cddr desc)))
;;                           desc))))

;; (define-struct-with-required-slots (abstract-array-struct)
;;   (storage      nil :required t)
;;   (dimensions   nil :required t :type list)
;;   (element-type t   :required t)
;;   (rank         nil :required t :type (integer 0 #.array-rank-limit))
;;   (total-size   nil :required t :type (integer 0 #.array-total-size-limit)))

;; (define-struct-with-required-slots (abstract-array-struct)
;;   (storage      nil :required t)
;;   (dimensions   nil :required t :type list)
;;   (element-type t   :required t)
;;   (rank         nil :required t :type (integer 0 #.array-rank-limit))
;;   (total-size   nil :required t :type (integer 0 #.array-total-size-limit)))

;; (defun struct-rank (obj)
;;   (declare (optimize speed))
;;   (abstract-array-struct-rank obj))

;; (defun clos-rank (obj)
;;   (declare (optimize speed))
;;   (slot-value obj 'rank))

;; (defun my-array-rank (obj)
;;   (declare (optimize speed )))

;; (let ((a (make-abstract-array-struct :rank 2
;;                                      :dimensions '(1 2)
;;                                      :total-size 2
;;                                      :element-type t
;;                                      :storage nil)))
;;   (time (loop repeat 10000000 do (struct-rank a))))

;; (defun vector-rank (obj)
;;   (declare (optimize speed)
;;            (type (simple-array t 1) obj))
;;   (elt obj 0))

;; (let ((a (vector 2)))
;;   (time (loop repeat 10000000 do (vector-rank a))))

;; (let ((a (make-instance 'abstract-array :rank 2
;;                                         :dimensions '(1 2)
;;                                         :total-size 2
;;                                         :element-type t
;;                                         :storage nil)))
;;   (time (loop repeat 10000000 do (clos-rank a))))

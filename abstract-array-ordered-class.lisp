(in-package :abstract-arrays)

;;; All thanks to
;;; https://git.mfiano.net/mfiano/zed/src/branch/master/src/util-ordered-class.lisp !

(defclass abstract-array-class (standard-class)
  ((order :reader order
          :initarg :order
          :initform nil)))

(defmethod closer-mop:validate-superclass
    ((class abstract-array-class) (superclass standard-class))
  t)

(defun plist-keys (list)
  (loop :for item :in list
        :for i :from 0
        :if (evenp i)
          :collect item))

(defun plist-values (list)
  (loop :for item :in list
        :for i :from 0
        :if (oddp i)
          :collect item))

(defun plist-remove (list &rest keys)
  (let ((list (copy-list list)))
    (loop :for key :in keys
          :do (remf list key))
    list))

(defmethod closer-mop:compute-slots ((class abstract-array-class))
  (let* ((order (order class))
         (length (length order)))
    (sort (copy-list (call-next-method))
          (lambda (x y)
            (< (or (position x order) length)
               (or (position y order) length)))
          :key #'closer-mop:slot-definition-name)))

(defun collect-ordered-class-accessor-names (slot-options accessor-type)
  (let ((filtered-keys (remove accessor-type (plist-keys slot-options))))
    (plist-values (apply #'plist-remove slot-options filtered-keys))))

(defun generate-ordered-class-fast-accessors (class-name slots order)
  (alexandria:mappend
   (lambda (slot)
     (destructuring-bind (slot-name &rest slot-options &key type inline
                                                         polymorph &allow-other-keys)
         slot
       (when (find slot-name order)
         (let ((access
                 `(closer-mop:standard-instance-access
                   ,class-name
                   ,(+ #+sbcl 0
                       #+ccl 1
                       #-(or sbcl ccl)
                       (progn
                         (warn "Not sure how to handle STANDARD-INSTANCE-ACCESS")
                         0)
                       (position slot-name order)))))
           `(,@(mapcan
                (lambda (x)
                  `(,@(if polymorph
                          `((defpolymorph (,x :inline ,inline) ((,class-name ,class-name))
                                ,(or type t)
                              ,access))
                          `((declaim ,@(when inline `((inline ,x)))
                                     ,@(when type `((ftype (function (,class-name) ,type) ,x))))
                            (defun ,x (,class-name)
                              ,access)))))
                (collect-ordered-class-accessor-names slot-options :reader))
             ,@(mapcan
                (lambda (x)
                  `(,@(if polymorph
                          `((defpolymorph (,x :inline ,inline) ((,class-name ,class-name))
                                ,(or type t)
                              (setf ,access value)))
                          `((declaim ,@(when inline `((inline ,x)))
                                     ,@(when type `((ftype (function (,class-name) ,type) ,x))))
                            (defun ,x (value ,class-name)
                              (setf ,access value))))))
                (collect-ordered-class-accessor-names slot-options :writer))
             ,@(mapcan
                (lambda (x)
                  `(,@(if polymorph
                          `((defpolymorph (,x :inline ,inline) ((,class-name ,class-name))
                                ,(or type t)
                              ,access)
                            (defpolymorph ((setf ,x) :inline ,inline)
                                ((value ,(or type t)) (,class-name ,class-name))
                                ,(or type t)
                              (setf ,access value)))
                          `((declaim ,@(when inline `((inline ,x)
                                                      (inline (setf ,x))))
                                     ,@(when type `((ftype (function (,class-name) ,type) ,x)
                                                    (ftype (function (,type ,class-name) ,type)
                                                           (setf ,x)))))
                            (defun ,x (value ,class-name)
                              (setf ,access value))
                            (defun (setf ,x) (value ,class-name) (setf ,access value))))))
                (collect-ordered-class-accessor-names slot-options :accessor)))))))
   slots))

(defun generate-ordered-class-slot-specifiers (slots order)
  (mapcar
   (lambda (x)
     (destructuring-bind (slot-name . slot-options) x
       (let ((to-remove (if (find slot-name order)
                            '(:inline :reader :writer :accessor :polymorph)
                            '(:inline))))
         (cons slot-name (apply #'plist-remove slot-options to-remove)))))
   slots))

(defun generate-ordered-class-options (order options)
  `((:metaclass abstract-array-class)
    (:order ,@order)
    ,@(remove-if (lambda (x) (member x '(:metaclass :order))) options :key #'car)))

(defmacro define-ordered-class (name super-classes &body (slots . options))
  (let ((order (cadr (find :order options :key #'car))))
    `(progn
       (defclass ,name ,super-classes
         ,(generate-ordered-class-slot-specifiers slots order)
         ,@(generate-ordered-class-options order options))
       ,@(generate-ordered-class-fast-accessors name slots order))))



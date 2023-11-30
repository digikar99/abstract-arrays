(defsystem "abstract-arrays"
  :author "Shubhamkar B. Ayare (shubhamayare@yahoo.co.in)"
  :version "0.3.0"
  :description "Julia's AbstractArray like abstraction for Common Lisp."
  :licence "MIT"
  :depends-on ("alexandria"
               "closer-mop"
               "compiler-macro-notes"
               "introspect-environment"
               "optima"
               "peltadot"
               "trivial-types")
  :serial t
  :components ((:file "pre-package")
               (:file "package")
               (:file "abstract-array-ordered-class")
               (:file "abstract-array")
               (:file "remaining-protocol")
               (:file "specializations")
               (:file "parametric-types")
               (:file "conditions")))

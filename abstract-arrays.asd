(defsystem "abstract-arrays"
  :author "Shubhamkar B. Ayare (shubhamayare@yahoo.co.in)"
  :version "0.2.4"
  :description "Julia's AbstractArray like abstraction for Common Lisp."
  :licence "MIT"
  :depends-on ("polymorphic-functions"
               "alexandria"
               (:feature :extensible-compound-types "extensible-compound-types-cl")
               (:feature :extensible-compound-types "optima")
               "closer-mop"
               "introspect-environment"
               "trivial-types")
  :serial t
  :components ((:file "pre-package")
               (:file "package")
               (:file "abstract-array-ordered-class")
               (:file "abstract-array")
               (:file "remaining-protocol")
               (:file "parametric-types")
               (:file "specializations")
               (:file "conditions")))

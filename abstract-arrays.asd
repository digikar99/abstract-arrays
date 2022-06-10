
(defsystem "abstract-arrays"
  :author "Shubhamkar B. Ayare (shubhamayare@yahoo.co.in)"
  :version "0.1.1"
  :description "Julia's AbstractArray like abstraction for Common Lisp."
  :licence "MIT"
  :depends-on ("polymorphic-functions"
               "alexandria"
               (:feature :extensible-compound-types "extensible-compound-types-cl")
               (:feature :extensible-compound-types "optima")
               "closer-mop"
               "introspect-environment")
  :serial t
  :components ((:file "pre-package")
               (:file "package")
               (:file "abstract-array-ordered-class")
               (:file "abstract-array")
               (:file "remaining-protocol")
               (:file "specializations-cl" :if-feature (:not :extensible-compound-types))
               (:file "specializations-excl" :if-feature :extensible-compound-types)))


(defsystem "abstract-arrays"
  :author "Shubhamkar B. Ayare (shubhamayare@yahoo.co.in)"
  :version "0.1.0"
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
               #-extensible-compound-types
               (:file "specializations-cl")
               #+extensible-compound-types
               (:file "specializations-excl")))

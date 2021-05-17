
(defsystem "abstract-arrays"
  :depends-on ("adhoc-polymorphic-functions"
               "alexandria"
               "closer-mop"
               "introspect-environment")
  :serial t
  :components ((:file "pre-package")
               (:file "package")
               (:file "abstract-array-ordered-class")
               (:file "abstract-array")
               (:file "remaining-protocol")
               (:file "specializations")))

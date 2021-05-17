
(defsystem "abstract-arrays"
  :depends-on ("adhoc-polymorphic-functions"
               "alexandria"
               "closer-mop"
               "introspect-environment")
  :serial t
  :components ((:file "pre-package")
               (:file "package")
               (:file "protocol")
               (:file "abstract-arrays")
               (:file "specializations")))

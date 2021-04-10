
(defsystem "abstract-arrays"
  :depends-on ("adhoc-polymorphic-functions"
               "alexandria"
               "introspect-environment")
  :serial t
  :components ((:file "package")
               (:file "abstract-arrays")
               (:file "specializations")))

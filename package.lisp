(cl:in-package #:abstract-arrays/defpackage)
(peltadot/utils:defpackage :abstract-arrays
  (:use #:peltadot)
  #.(cons :shadow (export-symbols))
  #.(cons :export (export-symbols))
  (:local-nicknames
   (:polymorphic-functions #:peltadot/polymorphic-functions)
   (:traits #:peltadot-traits-library)))

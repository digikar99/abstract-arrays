(cl:in-package #:abstract-arrays/defpackage)
(polymorphic-functions.defpackage:defpackage :abstract-arrays
  #+extensible-compound-types
  (:use :extensible-compound-types-cl)
  #-extensible-compound-types
  (:use :cl)
  (:shadowing-import-exported-symbols :polymorphic-functions)
  #.(cons :shadow (export-symbols))
  #.(cons :export (export-symbols)))

(in-package #:abstract-arrays)

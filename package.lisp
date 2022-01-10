(cl:in-package #:abstract-arrays/defpackage)
(polymorphic-functions.defpackage:defpackage :abstract-arrays
  (:use :cl)
  (:shadowing-import-exported-symbols :polymorphic-functions)
  #.(cons :shadow (export-symbols))
  #.(cons :export (export-symbols)))

(in-package #:abstract-arrays)

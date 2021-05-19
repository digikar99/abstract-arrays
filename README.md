
# abstract-arrays

This system/library provides an `abstract-array` package with:

> Beta

- a metaclass `abstract-array-class`
- a class `abstract-array` with slots
  - storage
  - dimensions
  - element-type
  - rank
  - total-size

- slot readers:
  - array-storage
  - array-dimensions
  - array-rank
  - array-element-type
  - array-total-size

- array operators:
  - aref and (setf aref)
  - row-major-aref and (setf row-major-aref)

- predicate:
  - arrayp

- compile time facilities:
  - define-array-specializations
  - define-array-specialization-type
  - array-type-element-type
  - array-type-rank

> Alpha: Will potentially be removed

- define-array-class
- array operators:
  - array-storage-ref
  - array-storage-set


The slot-readers and array-operators are implemented using [adhoc-polymorphic-functions](https://github.com/digikar99/adhoc-polymorphic-functions/) to allow for [more-or-less] portable inlining and installation of compiler-macros wherever appropriate. An attempt has also been made to optimize slot accesses using ordered-class discovered at [mfiano/zed](https://git.mfiano.net/mfiano/zed/src/branch/master/src/util-ordered-class.lisp).

The compile time facilities allows one to define array specialization types using `deftype` that specialize on element-type and rank - without a corresponding explosion in the number of predicates. The specialization is done using a systematic arrangement of predicates that check for element-type or rank. These should also obey `subtypep` due to their arrangement.

The system also wraps around the CL symbols with the same names. Thus, for example, `abstract-arrays:array-dimensions` when compiled with appropriate type declarations is as efficient as `cl:array-dimensions`.

## Installation

Follow the instructions at https://github.com/digikar99/adhoc-polymorphic-functions/#getting-it-from-ultralisp

## Usage

1. Firstly subclass `abstract-array-class` to create the metaclass `my-array-class` for your own array.
2. Subclass `abstract-array` class with metaclass as `my-array-metaclass` to create the class* `my-array`. This `abstract-array` class may have additional slots as per its requirements. It is recommended that users use `define-array-class` macro for this purpose as this also defines the order of slots appropriately.
3. Implement polymorphs for `aref`, `(setf aref)`, `row-major-aref` and `(setf row-major-aref)`.

The system of metaclass and class was chosen as it aids in customizing the behavior of `my-array`. For an extensive example, see [dense-arrays/src/protocol.lisp](https://github.com/digikar99/dense-arrays/tree/main/src/protocol.lisp).

## To Contribute

- Create a sparse-array class with as many array operations as possible :)
- Use abstract-arrays and/or [dense-arrays](https://github.com/digikar99/dense-arrays) and report any issues!

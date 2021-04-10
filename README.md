
# abstract-arrays

Why adhoc-polymorphic-functions? 

IIUC, a generic-function that specializes on both `abstract-array` and a child class/structure of `abstract-array` (eg. `dense-array`) cannot be sealed, and thus is not amenable to inline optimization by fast-generic-functions.

static-dispatch, as of yet, provides no compiler notes for optimization purposes.

This is apart from the fact that as of SBCL 2.1.1, there is no way to specialize generic-functions on specialized arrays.

## Dependencies outside quicklisp

- [adhoc-polymorphic-functions](https://github.com/digikar99/adhoc-polymorphic-functions/)
- my copy of [trivial-types](https://github.com/digikar99/trivial-types/) (original has been archived by the author)

## Documentation

TODO (Raise an issue if this interests you, so I can consider increasing the priority for documentation. An example is [dense-arrays/src/package.lisp](https://github.com/digikar99/dense-arrays/blob/main/src/package.lisp).)


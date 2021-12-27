# cascade

[![Clojars Project](https://img.shields.io/clojars/v/town.lilac/cascade.svg)](https://clojars.org/town.lilac/cascade)
[![cljdoc badge](https://cljdoc.org/badge/town.lilac/cascade)](https://cljdoc.org/d/town.lilac/cascade)


Cascade is a library of continuation-passing, thunk producing versions of many
Clojure core functions.

The goal is to allow essentially unbounded recursion and mutual recursion of
seq operations. This means that the seq operations in this library must not
use the call stack. Instead, they use a combination of continuation-passing to
ensure that operations can always be in the tail position and trampolining to
ensure that operations do not use the call stack.

This provides the ability to write recursive algorithms that work on very nested
data structures in Clojure(Script) using familiar operations.

## cascade.core

`cascade.core` aims to cover
- seq operations: reduce, transduce, into, and common transducer-producing fns
- CPS fn composition: identity, complement, comp

All seq operations can be passed a continuation as first argument, which will
be called on completion of the operation, and returns a thunk (0-arity
function) which can be called to begin the operation. A thunk will return
either another thunk, which can be called to continue the operation, or the
result. Thus, they are meant to be used with `clojure.core/trampoline`.

If a continuation is not passed in to most seq operations, it is assumed you
want to run the operation eagerly and will trampoline for you, returning the
result.

### Transducers

Transducer-producing functions like `map`, `filter`, etc. take functions which
accept a continuation and the element of the sequence they are operating on, and
should call the continuation with the result instead of returning it.

When passed a single function, they return a transducer for use with
`cascade.core/transduce` and `cascade.core/into`.

When passed a function and a collection, they will eagerly execute the operation
using `trampoline` and return the result.

When passed a continuation, a function and a collection, they will return a
thunk, meaning it can be trampolined.

## cascade.hike

What is a hike, but a really long walk?

`cascade.hike` is like `clojure.walk`, but defines `walk` in a way that supports
walking very large, nested data structures without using the call stack.

Defines recursive tree operations for Clojure data structures. Functions in
this namespace take any data structure (list, vector, map, set, seq) and
traverses those forms.

`cascade.hike/walk` is a generic tree walker that uses continuation-passing
and returns thunks. It takes any data structure, calls a function with a
continuation on every element, and uses the value passed into the continuation
in place of the original. This makes it easier to write recursive search-and-replace
functions for very nested data that do not use the call stack, as shown in the
rest of the functions in this namespace.

## License

Copyright © 2021 Will Acton. Distributed under the EPL 2.0.

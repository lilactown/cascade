## Changelog

## UNRELEASED

### Added

* `cascade.core/reduce-kv`
* `cascade.hike/transform-keys`

### Breaking

* `reduce`, `transduce`, `into` & `map-into` now have the same order behavior as clojure core,
i.e. they no longer call `reverse` on lists & seqs on completion.
This means that e.g. `(map-into (cont-with inc) () (range 5))` now returns `(5 4 3 2 1)`
instead of `(1 2 3 4 5)`.

## 1.2.0

### Added

* Reducing functions (`reduce`, `transduce`, `into`) now respect `reduced`
* `take`, `take-while`, `drop`, and `drop-while`
* multi-collection `map` arity
* `cat` transducer and `mapcat`

## 1.1.1

### Added

Extend `IEqualWithContinuation` to `nil` and `Object` in order to remove
`satisfies?` call in `eq`, which should improve performance.

## 1.1.0

### Added

Docstrings to `cascade.core/eq` and `IEqualWithContinuation`.

### Breaking

Reversed arguments passed to `-eq` method.

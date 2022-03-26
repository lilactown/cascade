## Changelog

## UNRELEASED

### Added

* `cascade.hilke/transform-keys`

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

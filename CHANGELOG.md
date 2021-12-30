## Changelog

## 1.1.1

### Added

Extend `IEqualWithContinuation` to `nil` and `Object` in order to remove
`satisfies?` call in `eq`, which should improve performance.

## 1.1.0

### Added

Docstrings to `cascade.core/eq` and `IEqualWithContinuation`.

### Breaking

Reversed arguments passed to `-eq` method.

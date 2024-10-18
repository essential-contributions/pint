## Appendix D: Known Issues and Missing Features

### Known Issues

- [#913](https://github.com/essential-contributions/pint/issues/913): type checker sometimes misses
  type errors in `const` initializers.
- [#908](https://github.com/essential-contributions/pint/issues/908): some array sanity checks are
  not performed for `const` initializers.

### Missing Features

- Strings
- Dynamic arrays, i.e., array types where the size is a value that is not known at compile time.
- Morphisms such that `map`, `fold`, `filter`, etc. that will allow morphing arrays.
- Storage vectors

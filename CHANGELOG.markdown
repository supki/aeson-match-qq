next
====

  * Added `any` type for consistency.

  * Duplicate keys in objects are handled ~~properly~~ better (https://github.com/supki/aeson-match-qq/pull/40)

1.7.0
====

  * Removed some of conditional compilation, by no longer pretending that
  we provide `liftTyped` for `Matcher`.

  * Fixed yet another embarrassing parser bug (https://github.com/supki/aeson-match-qq/pull/33)

  * Implemented `[...]` and `{...}` as shortcuts to `_ : array` and `_ : object` respectively (https://github.com/supki/aeson-match-qq/pull/34)

  * Some work has been done on making match failures output understandable error messages (https://github.com/supki/aeson-match-qq/pull/37, https://github.com/supki/aeson-match-qq/pull/38, https://github.com/supki/aeson-match-qq/pull/39)

1.6.1
=====

  * Fixed another parser problem (https://github.com/supki/aeson-match-qq/pull/27)

1.6.0
=====

  * Clarified ans simplified API some more.

1.5.3
=====

  * Minuscule Haddock improvements

1.5.2
=====

  * Add `prettyError`, a pretty printer for `Error`s (https://github.com/supki/aeson-match-qq/pull/24)

1.5.1
=====

  * GHC 9.2 compatibility

1.5.0
=====

  * Streamlined API.

  * Sprinkled some CPP to support clients that have not been made compatible
    with Aeson 2.0 yet, so that they do not have to use a separate release (1.3.x)

1.4.3
=====

  * Improved matching errors for embedded Haskell values (https://github.com/supki/aeson-match-qq/pull/18)

  * Separate error types for type- and value- level mismatches (https://github.com/supki/aeson-match-qq/pull/18)

  * Case insensitive strings (https://github.com/supki/aeson-match-qq/pull/17)

1.4.2
=====

  * Unordered arrays (https://github.com/supki/aeson-match-qq/pull/15)

1.4.1
=====

  * Proper Unicode support (https://github.com/supki/aeson-match-qq/pull/14)

1.4.0
=====

  * Aeson 2.0 compatibility

1.3.0
=====

  * GHC 9.0 compatibility

1.2.0
=====

  * Fixed a couple of parsing edge cases (https://github.com/supki/aeson-match-qq/pull/11)

1.1.0
=====

  * Optionally typed holes (https://github.com/supki/aeson-match-qq/pull/9)

  * Fixed spurious validation errors (https://github.com/supki/aeson-match-qq/issues/7)

1.0.0
=====

  * Initial release.

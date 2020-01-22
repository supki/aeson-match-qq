aeson-match-qq
==============

Declarative JSON matchers.

Motivation
----------

When testing an HTTP service that spouts JSON documents, it's often inconvenient to write out the whole response as
the expected value.  Sometimes, you only care about a couple specific properties, or simply want to ensure that the
response has certain structure.  This package provides a quasiquoter to define declarative matchers using a variation
of the familiar syntax.

Features
--------

### Basic JSON construction

Since this package is heavily inspired by [aeson-qq][0], the parser tries to follow its behavior and has support for
optional quotes for simple keys and the `#{exp}` syntax for Haskell expression interpolation. It currently does not support
variable keys though and there are no plans to add them.

### Spread-like syntax for arrays and objects

This syntax allows you to match only the part of the structure you care about:

`[match| [1, 2, 3, ...] |]` matches arrays starting with `[1, 2, 3]`  
`[match| {foo: 1, bar: 2, ...} |]` matches objects that are supersets of `{foo: 1, bar: 2}`

### Holes

Holes are placeholders that match anything:

`[match| _ |]` matches any JSON document  
`[match| {foo: _} |]` matches any object that has the `foo` property.

If a hole is named, its value will be returned from `match`

`[match| {foo: {bar: _n} |]` will return `{n: 4}` if matched with `{foo: {bar: 4}}`

  [0]: https://hackage.haskell.org/package/aeson-qq

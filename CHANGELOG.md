# Revision history for schemas
## 0.4.0.2 -- 2020-04-28
* Fix bug in `instance HasSchema Either`

## 0.4.0.1 -- 2020-01-01
* Replace breadth-first search with a greedy depth-first search

## 0.4.0 -- 2019-12-20
* Handle non-termination as an effect in decoding
* Union api made more consistent across tagged and non tagged
* (internal) More principled TypedSchema ADT based on Sum Profunctors
* Total version of 'liftPrism'
* 'theSchema' renamed to 'schemaFor'
* Bugfixes for decoding

## 0.3.0.2 --  2019-10-29
* Show circular schemas

## 0.3.0.1 --  2019-10-25
* Fix a bug that made OpenApi2 generation diverge.

## 0.3.0 --  2019-10-23
* Fixed a bug in isSubtypeOf for unions
* Fixed exponential performance (#3)
* Changed the representation of untyped schemas to remove Alternatives
  Alternatives are only possible on typed schemas
* Added support for recursive schemas (#1)

## 0.2.0.3 --  2019-10-13
* Bug fixes and performance improvements

## 0.2.0.2 --  2019-10-07
* Change the default schema for `Either` to handle both CamelCase and lowercase

## 0.2.0.1 --  2019-10-02
* Fixed subtyping relation for arrays

## 0.2.0 --  2019-09-29
* Add OpenApi2 encoding
* Change the `Semigroup` instance for typed schemas
* Trimmed down dependencies slightly

## 0.1.1.0 --  2019-09-28
* Fixed several bugs in `isSubtypeOf` and `encodeWith`
* Better error messages when encoding with a partial schema fails
* New: 'liftPrism' and 'oneOf'

## 0.1.0.0 -- 2019-09-25

* First version. Released on an unsuspecting world.

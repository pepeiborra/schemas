[![CI](https://travis-ci.com/pepeiborra/schemas.svg)](https://travis-ci.org/pepeiborra/threepenny-editors)
[![Hackage](https://img.shields.io/hackage/v/schemas.svg)](https://hackage.haskell.org/package/threepenny-editors)
# schemas

schemas is a Haskell-centric serialization library written with versioning in mind. Since a schema is a first-class citizen, it can be serialized, reasoned about, and transmitted together with the data. Serialization and deserialization work better when the source schema is provided, and versioning is accomplished by checking that the two schemas are related by a subtyping relation. This alleviates the need to keep old versions of datatypes around.

Consider a schema modification that adds a field. To support upgrading old documents to the new schema, the only requirement is that the new field is optional. Downgrading is easy too, simply omit the new field. Conversely, a schema modifcation that removes a field supports trivial upgrades but the removed field must be optional to support downgrading. Changing the type of a field is supported in as much as the old and new types are relatable. Field renaming is not supported. More importantly, all these changes are defined by a schema relation, and the library provides a predicate to check whether the relation holds.

schemas can also be used in a GraphQL-like fashion, allowing clients to request a subset of the schema. This comes up specially when working with recursive schemas involving cyclic data.

## Features
* schemas are first-class citizens and can be serialized,
* schema construction is statically typed,
* versioning is driven by a subtyping relation, no need for version numbers,
* Serialization to JSON only currently

## Why schemas

A quick seach in Hackage reveals a large number of libraries about schemas, including [json-schema], [hjsonschema], [aeson-schema], [aeson-schemas] and [hschema], amongst others.
There's undoubtedly a large amount of overlapping amongst all these libraries, so the immediate question is, why introduce another one ? 

This library is a re-implementation of an encoding library found in the Strats codebase at Standard Chartered Bank, the origins of which is go back a few years in time.
It predates other libraries that accomplish a similar task, including most of the ones mentioned before.
The approach has worked well but the codebase is showing its age and limitations, notably the lack of decoding capabilities.
This library extends the original approach with decoding and alternatives, hopefully keeping the good parts like the subtyping relation, intact.
 
[json-schema]: http://hackage.haskell.org/packages/json-schema
[hjsonschema]: http://hackage.haskell.org/packages/hjsonschema
[aeson-schema]: http://hackage.haskell.org/packages/aeson-schema
[aeson-schemas]: http://hackage.haskell.org/packages/aeson-schemas
[hschema]: http://hackage.haskell.org/packages/hschema


## Subtyping relation

schemas relies on a simple subtyping relation between schemas to perform value conversions. The basic idea is that these conversions are fully guided by the source and target schemas and involve only simple projections and injections:
1. Projecting a subset of the source record fields.
2. Turning a source field of type `A` into a target field of type `Array A`.

For more concrete details on the subtyping relation check the definition of `isSubtypeOf`. This function returns a witness, i.e. a conversion function, whenever the relation holds.

Versioning makes use of this subtyping relation as follows. Downgrading a value `v_2 :: T_2` into a previous version `T_1` is accomplished via the witness of `schema(T_1) > schema(T_2)`. Similarly, upgrading a `v_1 :: T_1` message into a newer version `T_2` can be accomplished via the witness of `schema(T_1) < schema(T_2)`. Therefore, a type `T_1` can only be replaced by a type `T_2` in an downgrade-compatible way if `schema(T_1) > schema(T_2)`; if upgrades are required, then `schema(T_1) < schema(T_2)` is required too.

The `<` relation is reflexive and transitive, but importantly not asymmetric or antisymmetric: it can be that both `T_1 < T_2` and `T_2 < T_1` and yet they are not the same type. For example, given a `S_2` schema that adds a required field to `S_1`, we would have that `S_1 > S_2` but not `S_1 < S_2`. However, if new the field was optional, then we would have `S_1 < S_2` too. In such case, we say that `S_1 ~ S_2` because they only differ on optional fields.
For example, given a `S_3` schema that removes a field from `S_2`, we have:
- `S_2 < S_3` therefore we can upgrade `S_2` values to `S_3`
- `S_2 ~ S_3` if the removed field is optional, in which case we can also downgrade `S_3` values to `S_2`

The `~` relation is an equivalence class, i.e. it is reflexive, symmetric and transitive.

## Alternative encodings

Sometimes there is more than one way to encode a value. A field can be renamed or change its type, an optional field become mandatory, several fields can be merged into one, etc. Alternative encodings allow for backwards compatible schema evolution.
This library support alternative encodings via the `Monoid` instance for typed schemas and the `Alternative` instance for `RecordFields`. 

The schema `A|B` encodes a value in two alternative ways `A` and `B`. A message created with this schema may encodings A, B or both. 'encode' will always create messages with all the possible encodings. While messages with multiple alternative encodings are not desirable for serialization, the desired message can be carved out using the subtyping relation. All the following hold:
```
A < A|B (the coercion A -> A|B will produce a message with an A encoding)
B < A|B (the coercion B -> A|B will produce a message with a  B encoding)
A|B < A (the coercion A|B -> A will succeed only if the message contains an A encoding)
A|B < B (the coercoin A|B -> B will succeed only if the message contains a  B encoding)
```

Typed schemas implement a limited form of alternative encodings via the `Alternative` instance for record fields. In the future a similar 'Alternative' instance for union constructors could be added.

## Example
- [Person](example/Person.hs)
- [Person2](example/Person2.hs)
- [Person3](example/Person3.hs)

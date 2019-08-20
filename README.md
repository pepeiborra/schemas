# schemas
A library for schema-guided serialization of Haskell data types. 

## Features
* schemas are first-class citizens and can be reasoned about,
* schema construction is statically typed,
* versioning is driven by a subtyping relation, no need for version numbers,
* Serialization to JSON only currently

## Why schemas

schemas is a Haskell-centric serialization library written with versioning in mind. Since a schema is a first-class citizen, it can be serialized and reasoned about. Serialization and deserialization require a source and target schema, and versioning is accomplished by checking that the two schemas are related by a subtyping relation. There is no need to keep old versions of datatypes around nor to write code for upgrades/downgrades.

Consider a schema modification that adds a field. To support upgrading old documents to the new schema, the only requirement is that the new field is optional. Downgrading is easy too, simply omit the new field. Conversely, a schema modifcation that removes a field supports trivial upgrades but the removed field must be optional to support downgrading. Changing the type of a field is supported in as much as the old and new types are relatable. Field renaming is not supported. More importantly, all these changes are defined by a schema relation, and the library provides a predicate to check whether the relation holds.

schemas can also be used in a GraphQL-like fashion, allowing clients to request a subset of the schema. This comes up specially when working with recursive schemas involving cyclic data.

## Subtyping relation

schemas relies on a simple subtyping relation between schemas to perform value conversions. The basic idea is that these conversions are fully guided by the source and target schemas and involve only simple projections and injections:
1. Projecting a subset of the source record fields.
2. Turning a source field of type `A` into a target field of type `Array A`.

For more concrete details on the subtyping relation check the definition of `isSubtypeOf`. This function returns a witness, i.e. a conversion function, whenever the relation holds.

Versioning makes use of this subtyping relation as follows. Downgrading a value `v_2 :: T_2` into a previous version `T_1` is accomplished via the witness of `schema(T_1) < schema(T_2)`. Similarly, upgrading a `v_1 :: T_1` message into a newer version `T_2` can be accomplished via the witness of `schema(T_1) > schema(T_2)`. Therefore, a type `T_1` can only be replaced by a type `T_2` in an downgrade-compatible way if `schema(T_1) < schema(T_2)`; if upggrades are required, then `schema(T_1) > schema(T_2)` is required too.

The `<` relation is reflexive and transitive, but importantly not asymmetric or antisymmetric: it can be that both `T_1 < T_2` and `T_2 < T_1` and yet they are not the same type. For example, given a `S_2` schema that adds a required field to `S_1`, we would have that `S_1 < S_2` but not `S_1 > S_2`. However, if new the field was optional, then we would have `S_1 > S_2` too. In such case, we say that `S_1 ~ S_2` because they only differ on optional fields.
For example, given a `S_3` schema that removes a field from `S_2`, we have:
- `S_2 > S_3` therefore we can upgrade `S_2` values to `S_3`
- `S_2 ~ S_3` if the removed field is optional, in which case we can also downgrade `S_3` values to `S_2`

The `~` relation is an equivalence class, i.e. it is reflexive, symmetric and transitive.


## Example
- [Person](example/Person.hs)
- [Person2](example/Person2.hs)
- [Person3](example/Person3.hs)

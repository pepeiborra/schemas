# schemas
A library for schema-guided serialization of Haskell data types.

## Features
* schemas are first-class citizens and can be reasoned about
* schema construction is statically typed
* painless versioning driven by a subtyping relation
* No type classes involved
* Serialization to JSON currently, other formats should be possible too.

## Why schemas

Since a schema is a first-class citizen, they too can be serialized and reasoned about. Serialization and deserialization require a source and target schema, and a simple form of versioning is accomplished by checking that the two schemas are related by a subtyping relation, witnessed by a coercion function.

## Example
- [Person](example/Person.hs)
- [Person2](example/Person2.hs)
- [Person3](example/Person3.hs)

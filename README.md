# Essa
Blacksmithing for Scala.
Essa helps with transformations between types, e.g. case classes or sealed traits - without macros or reflection.


## Usage

```scala
import essa.convert

case class A(x: Int, y: String)
case class B(x: Int, y: String)

val b: B = convert[B](A(42, "essa"))
```

## Features

- Convert non-nested product types
- Convert arbitrary nested product types
- Sumtype support
- Fields order
- Facility to use multiple transformations consecutively
- TODO: General type conversion
- TODO: Better error messages
- TODO: Specific field conversion
- TODO: Primitive type to Tagged type / value class conversion
- TODO: Effect type F[_] support for conversions
- TODO: Enum support
- TODO: Replace typetagging via .isInstanceOf by shapeless type tagging  
- TODO: Support for recursivly defined sumtypes

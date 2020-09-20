In typescript every value, even if all of those values have the same type, can have different implementations of interfaces

In Haskell, if you have a bunch of values of the same type, they always share their type class implementations
Always always

So for example, you have a bunch of values of type Int, but the methods of the Ord Int instance are always the same
You have a bunch of values of type String, but the methods of the Eq String instance are always the same
Instead of having a class, which implements the methods of the interface in its vtable (a technical name for the place the methods of a class are), then instantiating the class, making a bunch of objects with their own vtables
You have values which have types, and the type class instances belong to the types
The values do not "own" the vtables, in a sense
This distinction may seem subtle at the start because it relies on "type" and "class" being distinct and that distinction is not always familiar

The method of the type class, ==, takes two arguments, both of that type a

[] a, as a type, is the same as [a]

Foldable Pair can be read as "there is an instance of Foldable for Pair"

- -> \*

You can read \* as "type"

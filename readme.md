Q - are lists in Haskell linked lists?

interesting answer here
https://softwareengineering.stackexchange.com/questions/294983/why-do-haskell-and-scheme-use-singly-linked-lists/294984

example of laziness, and the power of it.

more documentation ->
:t get type info
:doc
:info
:h

a good explanation of type constructor by mlugg

A type constructor as we're using it here is just a type
Because we currently have nullary ones
However, the distinction becomes important for things like lists. In Java, List is not a valid type; it's templated, so you have to parameterize it over some other type, like List<Int>. In Haskell, we have a similar concept (but better) - we'd write that as List Int, and like in Java, List isn't a valid type by itself, but rather it's a type constructor
i.e. it's a thing that takes a type (the thing we want a list of) and builds a type from it (the actual list type)
But so far, we haven't defined any polymorphic types, so what the thing described as a type constructor is just a plain ol' type
Does that make any sense
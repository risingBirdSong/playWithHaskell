Q - are lists in Haskell linked lists?

interesting answer here
https://softwareengineering.stackexchange.com/questions/294983/why-do-haskell-and-scheme-use-singly-linked-lists/294984

example of laziness, and the power of it.

more documentation ->
:t get type info
:doc
:info
:h

question

I took a crack at writing the type for sum' with foldl (following along to Learn you a Haskell) as follows...

sum\_ :: (a -> [a]) -> a -> [a] -> [b]
sum' x xs = foldl (\acc x -> acc + x) 0 xs

and received the error ->

-- \* Couldn't match expected type `a -> [a]' -- with actual type `[a -> [a] -> [b]]'

which was interesting to me because it was the very type i wrote, but wrapped in a list
out of curiosity i pasted that actual type into my signature as follows

sum\_ :: (a -> [a]) -> a -> [a -> [a] -> [b]]

And as expected it was a recursed version of the previous error

-- \* Couldn't match expected type `a -> [a]' -- with actual type `[a -> [a -> [a] -> [b]]]'

and yes it was! so I erased my type and ran :t sum' to see the inferred type ->
sum' :: (Foldable t, Num a) => p -> t a -> a

very interesting... my question (I looked at the Foldable type and its definition is beyond my current understanding) is that this Foldable type, at least in the context of this sum' function... it is the structure for this recursive definition I was seeing previously?

[a -> [a -> [a -> [a] -> [b]]]] ?

How would you explain this recursive nesting in easy to understand terms?
And what is type p in this case? Is it the inline lamba defined in sum'

Sorry I know this is long but this seems to be an extremely important and interesting concept so I want to understand it.
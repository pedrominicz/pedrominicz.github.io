---
layout: post
permalink: /a-cryptic-haskell-expression
title: A cryptic Haskell expression
---

While reading [Ben Lynn's excellent Lambda calculus tutorial][1] I found the following mind boggling expression:

```
join (***) (sub x t) <$> rest
```

For context, `sub` is a function that takes three arguments (that is `sub x t` is a function that takes one argument) and `rest` is a list. But what is `join` or `***`? Thankfully Haskell has great documentation and tools. In this article we will use GHCi's `:i` and `:t` commands to figure this expression out.

A few `:t`s (abbreviation for `:type`) shows that `join` removes a monad layer and that `***` merges two `Arrow`s.

```
λ> :t join
join :: Monad m => m (m a) -> m a
λ> :t (***)
(***) :: Arrow a => a b c -> a b' c' -> a (b, b') (c, c')
```

Using `:i` (abbreviation for `:info`) we see that the `Arrow` type class inherits from the `Category` type class (aha!). And more useful information can be found below: `->` is an instance of `Arrow`!

```
λ> :i Arrow
class Category a => Arrow (a :: * -> * -> *) where
...
instance Arrow (->) -- Defined in ‘Control.Arrow’
```

As functions are the only `Arrow`s we have lying around, we can specialize the type of `***` as:

```
(***) :: (b -> c) -> (b' -> c') -> ((b, b') -> (c, c'))
```

(Redundant parenthesis added for clarity.)

But `join` acts on monads... perhaps...

```
λ> :i (->)
...
instance Monad ((->) r) -- Defined in ‘GHC.Base’
```

Bingo!

Once again, we rewrite the type of `***`.

```
(***) :: (->) (b -> c) ((->) (b' -> c') ((b, b') -> (c, c')))
```

Since `join` requires both monad layers to be equal, `(->) (b -> c)` and `(->) (b' -> c')` have to be of the same monad. Meaning our `join` is going to take `(b -> c) -> (b -> c) -> (b, b) -> (c, c)` into `(b -> c) -> (b, b) -> (c, c)`, or, more generally:

```
λ> :t join (***)
join (***) :: Arrow a => a b c -> a (b, b) (c, c)
```

`join (***)` takes a function of `a` to `b` to a function of pairs of `a` to pairs of `b`. It is equivalent to the following function:

    both :: (a -> b) -> (a, a) -> (b, b)
    both f (x, y) = (f x, f y)

I think it's interesting that Lynn decided to use `join (***)` instead of writing or importing a simple `both` function. It show familiarity with Haskell and category theory theory. But at the cost of readability.

---

As a bonus, here are a few cool things you can do with `join`.

```
λ> -- `join const` is the identity function.
λ> :t join const
join const :: a -> a
λ> join const 3
3
λ> -- `join (+)` is `(* 2)`
λ> join (+) 3
6
λ> join (+) 27
54
λ> -- `(>>= id)` is `join`
λ> :t (>>= id)
(>>= id) :: Monad m => m (m a) -> m a
```

`join` is also an alternative for `(>>=)` when defining a monad. `(>>=)` can be defined in terms of `join`.

```
λ> :t \x f -> join $ fmap f x
\x f -> join $ fmap f x :: Monad m => m a -> (a -> m b) -> m b
```

[1]: https://crypto.stanford.edu/~blynn/lambda/hm.html

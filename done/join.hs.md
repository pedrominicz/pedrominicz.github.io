---
permalink: /join
layout: default
---

# Understanding "join (\*\*\*)"

While reading [Ben Lynn's excellent Lambda calculus tutorial][1] I found the following mind boggling expression:

```
join (***) (sub x t) <$> rest
```

For context, `sub` is a function that takes three arguments (i.e. `sub x t` is a function that takes one argument) and `rest` is a list. At the time I didn't knew what either `join` or `***` do. But thankfully Haskell has great documentation and tools. In this article we will use GHCi's `:i` and `:t` commands to figure this expression out.

A few `:t`s (abbreviation for `:type`) shows that `join` removes a monadic layer and that `***` joins two `Arrow`s.

```
λ> :t join
join :: Monad m => m (m a) -> m a
λ> :t (***)
(***) :: Arrow a => a b c -> a b' c' -> a (b, b') (c, c')
```

Using `:i` (abbreviation for `:info`) we see that the `Arrow` typeclass inherits from the `Category` typeclass (aha!). But more useful information can be found below: `->` is an instance of `Arrow`!

```
λ> :i Arrow
class Category a => Arrow (a :: * -> * -> *) where
...
instance Arrow (->) -- Defined in ‘Control.Arrow’
```

So, in our case, we can rewrite the type of `***` as:

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

We can, once again, rewrite the type of `***` as:

```
(***) :: (->) (b -> c) ((->) (b' -> c') ((b, b') -> (c, c')))
```

Since `join` requires both monadic layers to be equivalent, `(->) (b -> c)` and `(->) (b' -> c')` have to be of the same monad. Meaning our `join` is going to take `(b -> c) -> (b -> c) -> (b, b) -> (c, c)` into `(b -> c) -> (b, b) -> (c, c)`, or, more generally:

```
λ> :t join (***)
join (***) :: Arrow a => a b' c' -> a (b', b') (c', c')
```

`join (***)` takes a function of `a` to `b` to a function of pairs of `a` to pairs of `b` and its equivalent to the following function.

    tuplify :: (a -> b) -> (a, a) -> (b, b)
    tuplify f (x, y) = (f x, f y)

For me its incredible to think that Lynn decided to use `join (***)` instead of writing a simple `tuplify` function. It show how familiar he is with Haskell and the surrounding theory.

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

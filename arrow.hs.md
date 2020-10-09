---
permalink: /arrow
layout: default
---

# Fun with the function arrow monad

One of the things that surprised me while studying Haskell is that `(->) a` is a monad. It seems reasonable that functions form a monad, but I don’t believe I would have thought of it myself. How can this monad be used? An example, namely, `join (***)`, was considered in [a previous post][1]. But when I think monads in Haskell I, think of `do` notation. In this post we will define the `(->) a` monad ourselves and explored a bit of the weirdness of its `do` notation.

    {-# LANGUAGE FlexibleInstances #-}
    {-# LANGUAGE UndecidableInstances #-}

    module Arrow where

    import Prelude hiding (Functor(..), Applicative(..), Monad(..))

We hide `Functor`, `Applicative`, and `Monad` because we will implement our own hierarchy, similar to the one before the [functor-applicative-monad][2] proposal. `FlexibleInstances` and `UndecidableInstances` will be necessary for later shenanigans.

First, we define `Functor` and an instance of it for our monad. `fmap` is just `(.)`, pretty lame.

    class Functor f where
      fmap :: (a -> b) -> f a -> f b

    instance Functor ((->) a) where
      fmap = (.)

This can be easily excused, however. Lists are another type constructor that have this problem (their `fmap` is just `map`, boring!), yet their monad instance simulates nondeterministic computations (awesome!).

Let's continue with the `Applicative` instance:

    class Functor f => Applicative f where
      pure :: a -> f a
      (<*>) :: f (a -> b) -> f a -> f b

    instance {-# OVERLAPPING #-} Applicative ((->) a) where
      pure = const
      f <*> g = \x -> f x (g x)

Things have certainly got a lot more interesting. `(<*>)` and `pure` are the S and K combinators from the SKI combinator calculus!

(The reason why the applicative instance of `(->) a` is `OVERLAPPING` will become clear later.)

With them, we can define the I combinator:

    k :: a -> b -> a
    k = pure

    i :: a -> a
    i = k <*> k

With applicative functors out of our way, we define a monad instance for `(->) a` which will give us access to that sweet `do` notation. Hopefully, things are about to get exciting.

    class Functor m => Monad m where
      return :: a -> m a
      (>>=) :: m a -> (a -> m b) -> m b

    instance Monad ((->) a) where
      return = const
      f >>= g = \x -> g (f x) x

Oh, no! Things have got kind of boring again! `(>>=)` is just the S combinator with shuffled parameter order! And indeed, `(>>=)` could have been implemented in terms of S and K:

```
instance Monad ((->) a) where
  return = const
  (>>=) = s (k (s (k (s (k (s s (s k))))) (s (s (k s) k)))) k
```

Yet another possible implementation (thanks to [Brad Neimann][3]):

```
instance Monad ((->) a) where
  return = const
  f >>= g = flip g <*> f
```

<!-- This is secretly using the default arrow monad. I believe there is an extension that allows one to replace `do` notation, similar to `ApplicativeDo`. -->
With the monad instance, we finally acquired `do` notation, which allows us to write some weird things:

    s :: (a -> b -> c) -> (a -> b) -> a -> c
    s x y = do
      x <- x
      y <- y
      return (x y)

which desugars to:

    s' :: (a -> b -> c) -> (a -> b) -> a -> c
    s' x y = x >>= \x -> y >>= \y -> return (x y)

which is just a default implementation of `(<*>)` for any given monad:

    instance {-# OVERLAPPABLE #-} (Functor m, Monad m) => Applicative m where
      pure = return
      f <*> x = f >>= \f -> x >>= \x -> return (f x)

This applicative instance is the reason for the extensions and the `OVERLAPPING` in the applicative instance of `(->) a`.

I didn't manage to think of any useful examples of `do` notation for the `(->) a` monad. If you know of any, please let me know on [Twitter][4]. I would love to see more of this unexpected monad.

---

Special thanks to the wonderful folks at the [Functional Programming Zulip][5].

[1]: /join
[2]: https://wiki.haskell.org/Functor-Applicative-Monad_Proposal
[3]: https://funprog.zulipchat.com/#narrow/stream/201385-Haskell/topic/Arrow.20monad.20and.20SK.20calculus/near/212781408
[4]: https://twitter.com/pedrominicz
[5]: https://funprog.zulipchat.com

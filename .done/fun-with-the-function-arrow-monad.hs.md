---
layout: default
permalink: /fun-with-the-function-arrow-monad
title: Fun with the function arrow monad
---

I was surprised when I learned that `(->) a` is a monad. It seems reasonable, but I wouldn't have figured that myself. How can this monad be used? An example was shown in [a previous post][1]. But when I think monads in Haskell I, think of `do` notation. In this post we will define the `(->) a` monad ourselves and explored a bit of the weirdness of its `do` notation.

    {-# LANGUAGE FlexibleInstances #-}
    {-# LANGUAGE UndecidableInstances #-}

    module Arrow where

    import Prelude hiding (Functor(..), Applicative(..), Monad(..))

Hide `Functor`, `Applicative` and `Monad` because we will implement them ourselves. `FlexibleInstances` and `UndecidableInstances` are necessary for later shenanigans.

First, we define `Functor` and an instance for `(->) a`.

    class Functor f where
      fmap :: (a -> b) -> f a -> f b

    instance Functor ((->) a) where
      fmap = (.)

`fmap` is just `(.)`. Pretty lame. This can be excused depending on what comes later. For example, `fmap` for lists is just `map` (boring!), but their monad instance allows us to model nondeterministic computations (awesome!).

Let's continue with `Applicative`:

    class Functor f => Applicative f where
      pure :: a -> f a
      (<*>) :: f (a -> b) -> f a -> f b

    instance Applicative ((->) a) where
      pure = const
      f <*> g = \x -> f x (g x)

Things got a lot more interesting. `(<*>)` and `pure` are the S and K combinators from combinatory logic!

With them, we can define the I combinator:

    k :: a -> b -> a
    k = pure

    i :: a -> a
    i = k <*> k

Finally, let's define a monad instance for `(->) a` which gives us access to `do` notation. Hopefully things are about to get exciting.

    class Functor m => Monad m where
      return :: a -> m a
      (>>=) :: m a -> (a -> m b) -> m b

    instance Monad ((->) a) where
      return = const
      f >>= g = \x -> g (f x) x

Quite interesting. `(>>=)` is very similar to the S combinator. It is not immediately obvious, but it does not give us any extra power. Indeed, `(>>=)` can be defined in terms of S and K:

```
instance Monad ((->) a) where
  return = const
  (>>=) = s (k (s (k (s (k (s s (s k))))) (s (s (k s) k)))) k
```

Yet another possible implementation (thanks to [Brad Neimann][2]):

```
instance Monad ((->) a) where
  return = const
  f >>= g = flip g <*> f
```

With the monad instance, we can use `do` notation and write some weird things.

    s :: (a -> b -> c) -> (a -> b) -> a -> c
    s x y = do
      x <- x
      y <- y
      return (x y)

The function above desugars to:

```
s :: (a -> b -> c) -> (a -> b) -> a -> c
s x y = x >>= \x -> y >>= \y -> return (x y)
```

which can be used as the default implementation of `(<*>)` for any monad:

    instance {-# OVERLAPPABLE #-} (Functor m, Monad m) => Applicative m where
      pure = return
      f <*> x = f >>= \f -> x >>= \x -> return (f x)

This instance is the why I enabled some extensions and didn't make `Applicative` a superclass of `Monad`.

Now, is this monad weird and unexpected? Or somehow familiar?

    type Reader r a = r -> a

    ask :: Reader a a
    ask = id

It is very familiar indeed. In particular, the `Reader` type synonym makes our implementation of the S combinator a lot more clear.

```
s :: Reader a (b -> c) -> Reader a b -> Reader a c
s x y = do
  x <- x
  y <- y
  return (x y)
```

[1]: /join
[2]: https://funprog.zulipchat.com/#narrow/stream/201385-Haskell/topic/Arrow.20monad.20and.20SK.20calculus/near/212781408
[3]: https://funprog.zulipchat.com

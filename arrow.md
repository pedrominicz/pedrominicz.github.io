---
permalink: /arrow
layout: default
---

# Fun with the Function Arrow Monad

One of the things that surprised me while learning Haskell is that `(->) a` is a monad. It was clear that it was possible as soon as I saw it, but I wouldn't have thought of it myself.

However, how can this monad be used in practice? One use would be to pass functions accept generic monads. An example of such functions, namely, `join` and `(***)`, was considered in [a previous post][1]. Another obvious example would be `forever` from `Control.Monad`.

But when I think monads in Haskell I think `do` notation. In this post we will define the `(->) a` monad ourselves and explored a bit of the
weirdness of its `do` notation.

    {-# LANGUAGE FlexibleInstances #-}
    {-# LANGUAGE UndecidableInstances #-}

    module Arrow where

    import Prelude hiding (Functor(..), Applicative(..), Monad(..))

We hide `Functor`, `Applicative`, and `Monad` because we will implement our own hierarchy, similar to the one before the Functor-Applicative-Monad proposal. `FlexibleInstances` and `UndecidableInstances` will be necessary for later shenanigans.

First, we define functors and an instance of functor for our monad. `fmap` for `(->) a` is pretty lame, it's just `(.)`:

    class Functor f where
      fmap :: (a -> b) -> f a -> f b

    instance Functor ((->) a) where
      fmap = (.)

This can be easily excused, however. Lists are another type constructor that have this problem (their `fmap` is just `map`, boring!), yet their monad instance represents non-deterministic computations (awesome!).

Let's continue with an applicative functor instance:

    class Functor f => Applicative f where
      pure :: a -> f a
      (<*>) :: f (a -> b) -> f a -> f b

    -- instance Applicative ((->) a) where
    --   pure = const
    --   f <*> g = \x -> f x (g x)

Things have certainly gotten a lot more interesting. `pure` and `(<*>)` are the S and K combinators from the SKI combinator calculus!

(The reason why the applicative instance of `(->) a` is commented will become clear later.)

With them we can define the I combinator:

    k :: a -> b -> a
    k = pure

    i :: a -> a
    i = k <*> k

With applicative functors out of our way, we define a monad instance for `(->) a` which will give us access to that sweet `do` notation. Hopefully things are about to get exciting.

    class Functor m => Monad m where
      return :: a -> m a
      (>>=) :: m a -> (a -> m b) -> m b

    instance Monad ((->) a) where
      return = const
      f >>= g = \x -> g (f x) x

Oh, no! Things have gotten kind of boring again! `(>>=)` is just the S combinator with shuffled parameter order!

However, we finally acquired `do` notation, which allows us to write some weird things:

    s :: (a -> b -> c) -> (a -> b) -> a -> c
    s x y = do
      x <- x
      y <- y
      return (x y)

which desugars to:

    s' :: (a -> b -> c) -> (a -> b) -> a -> c
    s' x y = x >>= \x -> y >>= \y -> return (x y)

which is just a possible default implementation of `(<*>)` for any given monad:

    instance (Functor m, Monad m) => Applicative m where
      pure = return
      f <*> x = f >>= \f -> x >>= \x -> return (f x)

This applicative functor instance is the reason for the extensions and why the applicative instance of `(->) a` was commented out. Since `(->) a` is a monad, and every monad is an applicative functor, defining applicative for `(->) a` would create an overlapping instance.

I didn't manage to think of any useful examples of `do` notation for the `(->) a` monad. If you know of any, please let me know on [Twitter][2]. I would love to see more of this amazing monad.

[1]: /join
[2]: https://twitter.com/pedrominicz

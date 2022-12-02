---
layout: post
permalink: /unification-in-75-lines-of-haskell
title: Unification in 75 lines of Haskell
---

Unification is key for type inference algorithms and is used as a computational primitive in logic programming languages such as Prolog or [μKanren][1]. A simple unification algorithm is easy to implement and gives great insight. In this post we will implement unification in about 75 lines of Haskell.

First, we will define the terms we will be unifying. Terms are trees. They are either a node (with zero or more children) or an unification variable.

    import Control.Applicative
    import Control.Monad.State
    import qualified Data.IntMap as IM

    data Term
      = Term String [Term]
      | Var Int
      deriving Show

Unification is straight forward. We recursively descend each node comparing them and substituting variables. If the nodes are different, we fail. If substituting a variable creates a cycle, we fail. That is it!

Things will become clearer when we start implementing. But before that, some machinery is required.

    type Binding = IM.IntMap Term

    type Unify a = StateT Binding Maybe a

Unification happens inside the `Unify` monad. A `Binding` keeps track of which variables correspond to which terms. Transforming `StateT` with `Maybe` gives us an [`Alternative`][2] instance which we use when unification fails.

    lookupVar :: Int -> Unify (Maybe Term)
    lookupVar x = gets (IM.lookup x)

    bind :: Int -> Term -> Unify ()
    bind x t = modify $ \s -> IM.insert x t s

Looking up variables and binding them is straight forward. `lookupVar` uses [`gets :: MonadState s m => (s -> a) -> m a`][3], which some may be unfamiliar with (I was a while ago).

One last helper function, the famous occurs check, which can be a major source of performance issues.

    occurs :: Int -> Term -> Bool
    occurs x (Term _ ts) = any (occurs x) ts
    occurs x (Var y)     = x == y

A variable occurs in a term if the term itself is that variable or if it occurs in any of its children.

The `apply` function applies (substitutes in) all bound variables to a term. If the term bound by a variable references it, that is, if a variable `occurs` in its bound term, we have a cycle and unification fails. Unbound variables are left untouched.

    apply :: Term -> Unify Term
    apply (Term t ts) = do
      ts <- traverse apply ts
      return $ Term t ts
    apply (Var x) = do
      t <- lookupVar x
      case t of
        Just t -> do
          guard $ not (occurs x t)
          t <- apply t
          bind x t
          return t
        Nothing -> return $ Var x

(`traverse` is like `map`, but it carries an effect with each computation. If you don't know about `Traversable`, look it up. It is one of my favorite type classes.)

Finally, we can define `unify`:

    unify :: Term -> Term -> Unify Term
    unify t1 t2 = do
      t1 <- apply t1
      t2 <- apply t2
      case (t1, t2) of
        (Var x1, Var x2)
          | x1 == x2 -> return $ Var x1
        (Var x, t) -> do
          bind x t
          return t
        (t, Var x) -> do
          bind x t
          return t
        (t1, t2) -> match t1 t2

Unifying a variable with itself always succeeds. If we do not check this, the variable is bound to itself making a cycle and causing future occurs checks to fail.

Each call to `bind` necessarily binds a free variable since all bound variables have been applied.

Now, we define `match`:

    match :: Term -> Term -> Unify Term
    match (Term t1 ts1) (Term t2 ts2) = do
      guard (t1 == t2)
      ts <- zipExact ts1 ts2
      ts <- traverse (uncurry unify) ts
      return $ Term t1 ts
    match _ _ = undefined

We check if both terms have the same names. Then we zip both argument lists. `zipExact` guarantees both terms have the same number of children. Finally, we unify every pair of terms and return the result. `match` should only be called when both arguments are terms.

    zipExact :: Alternative f => [a] -> [b] -> f [(a, b)]
    zipExact [] [] = pure []
    zipExact (x:xs) (y:ys) = ((x, y) :) <$> zipExact xs ys
    zipExact _ _ = empty

And that is it!

As an exercise, try adding unification to the implementation of μKanren shown in the [previous post][1]. [This Gist][4] contains examples of unification in action.

```
λ> term1 = Term "q" [Term "g" [Var 1], Term "f" [Var 2]]
λ> term2 = Term "q" [Term "g" [Term "f" [Var 3]], Var 1]
λ> evalStateT (unify term1 term2 >>= apply) IM.empty
```

I recommend reading Ben Lynn's [tutorial on type inference][5]. It was an important resource I used when I first implemented unification and Hindley-Milner type inference.

[1]: /embedding-a-logic-programming-language-in-haskell
[2]: https://hackage.haskell.org/package/base/docs/Control-Applicative.html#t:Alternative
[3]: https://hackage.haskell.org/package/mtl/docs/Control-Monad-State-Lazy.html#v:gets
[4]: https://gist.github.com/pedrominicz/c0b522b33e1e5be16785754d2060050c
[5]: https://crypto.stanford.edu/~blynn/lambda/hm.html

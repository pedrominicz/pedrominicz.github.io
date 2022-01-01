---
permalink: /fuse
layout: default
---

# Unification in 75 lines of Haskell

Unification is key for type inference algorithms and is used as a computational primitive in logic programming languages such as Prolog or [μKanren][1]. A simple algorithm is easy to implement and can give great insight. In this post we will implement unification in about 75 lines of Haskell.

First, we will define the terms we will be unifying. Terms are tree-like, they are either a node (with zero or more children) or a variable.

    import Control.Applicative
    import Control.Monad.State
    import qualified Data.IntMap as IM

    data Term
      = Term String [Term]
      | Var Int
      deriving Show

The process of unification is straight forward. First, bound variables are substituted in if there are no infinite cycles. Then, to unify two terms we recursively descend each node comparing them. If the nodes are different terms unification fails (e.g. `Term "a" []` doesn't unify with `Term "b" []`). If they are equal we match every children node of each term. If a free variable is encountered it is bound to the matching term.

Things will become clearer when we start implementing. But before that, some helper machinery is required.

    type Binding = IM.IntMap Term

    type Unify a = StateT Binding Maybe a

Unification will happen inside the `Unify` monad. The `Binding` state keeps track of the mapping from variables to terms. We make it an [`Alternative`][2] by transforming it with `Maybe` so we can easily fail unification.

    lookupVar :: Int -> Unify (Maybe Term)
    lookupVar v = gets (IM.lookup v)

    bind :: Int -> Term -> Unify ()
    bind v t = modify $ \s -> IM.insert v t s

Looking up variables and binding them is straight forward. `lookupVar` uses the [`gets :: MonadState s m => (s -> a) -> m a`][3] function, which some may be unfamiliar with (I was a while ago).

One last small helper function, the famous (infamous?) occurs check, which can be a major source of performance issues.

    occurs :: Int -> Term -> Bool
    occurs v (Term t ts) = any (occurs v) ts
    occurs v (Var v')    = v == v'

A variable occurs in a term if the term is that variable or if it occurs in any of its children.

The `apply` function applies (substitutes in) all bound variables to a term. If the term bound by a variable references it, that is, if a variable `occurs` in its bound term, an infinite chain is detected and unification fails.

    apply :: Term -> Unify Term
    apply (Term t ts) = do
      ts <- traverse apply ts
      return $ Term t ts
    apply (Var v) = do
      t <- lookupVar v
      case t of
        Just t -> do
          guard $ not (occurs v t)
          t <- apply t
          bind v t
          return t
        Nothing -> return $ Var v

Unbound variables are left untouched.

(`traverse` is like `map`, but it carries monadic side effects with each computation. If you are not familiar with with `Traversable` you should look it up. It is one of my favorite type classes.)

Finally, we can define `unify`:

    unify :: Term -> Term -> Unify Term
    unify t t' = do
      t  <- apply t
      t' <- apply t'
      case (t, t') of
        (Var v, Var v')
          | v == v' -> return $ Var v
        (Var v, t) -> do
          bind v t
          return $ Var v
        (t, Var v) -> do
          bind v t
          return $ Var v
        (t, t') -> match t t'

Unification of a variable with itself is successful. If we do not check this, the variable is bound to itself causing any attempts to `apply` it to fail the occurs check.

Note that every call to `bind` necessarily binds a free variable since all bound variables have been applied to the terms.

Now all we have to do it to define `match`:

    match :: Term -> Term -> Unify Term
    match (Term t ts) (Term t' ts') = do
      guard (t == t')
      ts <- zipExact ts ts'
      ts <- traverse (uncurry unify) ts
      return $ Term t ts
    match t t' = undefined

`match` should only be called when both arguments are terms.

First, we check if both terms have the same names. Then we zip both argument lists. `zipExact` guarantees both terms have the same arity and will be defined shortly. Lastly we unify every pair of terms and return the result.

    zipExact :: Alternative f => [a] -> [b] -> f [(a, b)]
    zipExact [] []         = pure []
    zipExact (x:xs) (y:ys) = ((x, y) :) <$> zipExact xs ys
    zipExact _ _           = empty

And that is it! Complete unification of nontrivial terms!

It is a good exercise to enhance the implementation of μKanren shown in the [previous post][1] with unification as shown here. Examples of unification in action may be found on [this Gist][4].

```
λ> term1 = Term "q" [Term "g" [Var 1], Term "f" [Var 2]]
λ> term2 = Term "q" [Term "g" [Term "f" [Var 3]], Var 1]
λ> evalStateT (unify term1 term2 >>= apply) IM.empty
```

---

I have to plug Ben Lynn's [tutorial on type inference][5] as it was a key resource I used when first implemented unification and Hindley-Milner type inference.

[1]: /logic
[2]: https://hackage.haskell.org/package/base/docs/Control-Applicative.html#t:Alternative
[3]: https://hackage.haskell.org/package/mtl/docs/Control-Monad-State-Lazy.html#v:gets
[4]: https://gist.github.com/pedrominicz/c0b522b33e1e5be16785754d2060050c
[5]: https://crypto.stanford.edu/~blynn/lambda/hm.html

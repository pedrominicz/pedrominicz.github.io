---
permalink: /unify
layout: default
---

# Unification in 75 lines of Haskell

Unification is key for type inference algorithms and is used as a computational primitive in logic programming languages such as Prolog or [μKanren][1]. A simple algorithm is easy to implement and can give great insight. In this post we will implement unification in about 75 lines of Haskell.

First we will define the terms we will be unifying. Terms are tree-like, they can be either a node with zero or more children or a variable.

    import Control.Monad.State
    import qualified Data.IntMap as IM

    data Term
      = Term String [Term]
      | Var Int
      deriving Show

Note that the user is _not_ supposed to create variables by hand (i.e. `v = Var 0`).

To unify two terms we recursively descend each node comparing them. If the nodes are different terms unification fails &mdash; e.g. `Term "a" []` doesn't unify with `Term "b" []`. If they are equal we match every children done of each term. If a bound variable is encountered it is substituted into the term (if there are no infinite cycles) and the process continues. If a free variable is encountered it is bound to the matching term.

Things will become clear when we start implementing. But before that, some helper machinery is required.

    data Binding = Binding
      { next     :: Int
      , bindings :: IM.IntMap Term
      }

    type Unify a = StateT Binding Maybe a

Unification will happen inside the `Unify` monad. The `Binding` state keeps track of the next free variable and a mapping from variables to terms. We also make it an [`Alternative`][2] by transforming it with `Maybe`.

    lookupVar :: Int -> Unify (Maybe Term)
    lookupVar v = gets (IM.lookup v . bindings)

    bind :: Int -> Term -> Unify ()
    bind v t = modify $ \s ->
      s { bindings = IM.insert v t (bindings s) }

Looking up variables and binding them is straight forward. `lookupVar` uses the [`gets`][3] function, which some may be unfamiliar with (I was a while ago), but which does the job just perfectly.

One last helper function and we are good to go. It is the infamous occurs check, which can be a major source of performance issues.

    occurs :: Int -> Term -> Bool
    occurs v (Term t ts) = any (occurs v) ts
    occurs v (Var v')    = v == v'

A variable occurs in a term if the term is that variable or if it occurs in any of its children.

The `apply` function applies all bound variables to a term. If the term bound by a variable references that variable, that is, if a variable `occurs` in its bound term, an infinite chain is detected and unification fails.

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

(`traverse` is like `map`, but it carries monadic side effects with each computation. If you are not familiar with with `Traversable` you should look into it, its currently my favorite typeclass.)

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

Unification with of a variable with itself is successful trivially. If we were not to check this, the variable would be bound to itself causing any subsequent attempts to `apply` it to fail in the occurs check.

Note that every call to `bind` necessarily binds a free variable, since all bound variables have been applied to the terms.

Now all we have to do it to define `match`:

    match :: Term -> Term -> Unify Term
    match (Term t ts) (Term t' ts') = do
      guard (t == t')
      ts <- zipExact ts ts'
      ts <- traverse (uncurry unify) ts
      return $ Term t ts
    match t t' = unify t t'

`match` should only be called when both arguments are terms, so we could have omitted the last pattern match.

First, we check if both terms have the same names. Then we zip both argument lists. `zipExact` guarantees both terms have the same arity and will be defined shortly. Lastly we unify every pair of terms and return the result.

A general definition of `zipExact` should the type `Alternative f => [a] -> [b] -> f [(a, b)]`, but for the sake of not having to import `Control.Applicative` we will specialize it for `Unify`.

    zipExact :: [a] -> [b] -> Unify [(a, b)]
    zipExact [] []         = return []
    zipExact (x:xs) (y:ys) = ((x, y) :) <$> zipExact xs ys
    zipExact _ _           = mzero

And that is it! Complete unification of nontrivial terms!

It is a good exercise to enhance the implementation of μKanren shown in the [previous post][1] with unification as shown here. Examples of unification in action may be found on [this Gist][4].

---

I have to plug Ben Lynn's tutorial on type inference [Outcoding UNIX geniuses][5] as it was a key resource I used when first implementing Hindley-Milner type inference. The other key resource was [the Wikipedia article][6].

[1]: /logic
[2]: https://hackage.haskell.org/package/base/docs/Control-Applicative.html#t:Alternative
[3]: https://hackage.haskell.org/package/mtl/docs/Control-Monad-State-Lazy.html#v:gets
[4]: https://gist.github.com/pedrominicz/c0b522b33e1e5be16785754d2060050c
[5]: https://crypto.stanford.edu/~blynn/lambda/hm.html
[6]: https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system

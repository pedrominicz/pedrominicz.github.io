---
permalink: /unify
layout: default
---

Unification is key for type inference algorithms and is used as a computational primitive in logic programming languages such as Prolog or [μKanren][1]. A simple algorithm is easy to implement and can give great insight. In this post we will implement unification in about 75 lines of Haskell.

First we will define the terms we will be unifying. Terms are tree-like, they can be either a node with zero or more children or a variable.

    import Control.Monad.State
    import qualified Data.IntMap as IM

    data Term
      = Term String [Term]
      | Var Int
      deriving Show

Note that the user is not supposed to create variables by hand (i.e. `v = Var 0`).

To unify two terms we recursively descend each node comparing them. If the nodes are different unification fails, e.g. `Term "a" []` doesn't unify with `Term "b" []`. If they are equal we match every children done of each term. If a bound variable is encountered it is substituted into the term (if there are no infinite cycles) and the process continues. If a free variable is encountered it is bound to the matching term.

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

[1]: /logic
[2]: https://hackage.haskell.org/package/base/docs/Control-Applicative.html#t:Alternative
[3]: http://hackage.haskell.org/package/mtl/docs/Control-Monad-State-Lazy.html#v:gets

---
permalink: /logic
layout: default
---

# Embedding a micro logic programming language in Haskell

One of the ideas behind this "blog" is to share the resources I find while studying. As such, this post exists not for its originality, but for me to have a chance to write about one of my favorite pieces of code. We will be implementing μKanren, a minimal logic embedded programming language, in Haskell. You can find the original Scheme implementation can be found on [the paper][1].

Before proceeding any further, I want to recommend [Seal Talt's μKanren implementation walkthrough][2]. We will be implementing almost the exact same thing, so feel free to follow his writeup instead.

Will begin defining our terms. For now terms either be logic variables or atomic constants.

    data Term
      = Var Int
      | Atom String
      deriving (Eq, Show)

Unification is straight forward given how simple our terms are. Before we define unification, however, we first need to define substitutions:

    type Subst = [(Int, Term)]

and a function for pruning a chain of variables:

    -- Assumes no cyclic chains.
    prune :: Subst -> Term -> Term
    prune s (Var v) =
      case lookup v s of
        Just t  -> prune s t
        Nothing -> Var v
    prune s t = t

The function just keeps looking up variables until it find an unbound one or a term.

The paper calls this function `walk`. I called it `prune` because its equivalent to the `fullprune` function in the [`unification-fd`][3] library.

Now we can define our unification function. Given a substitution and two terms, the function returns a new substitution under which the terms unify, if possible, and `Nothing` otherwise.

    unify :: Subst -> Term -> Term -> Maybe Subst
    unify s t t' = (++ s) <$> go (prune s t) (prune s t')
      where
      go t t' | t == t' = Just []
      go (Var v) t = Just [(v, t)]
      go t (Var v) = Just [(v, t)]
      go t t' = Nothing

Our terms are so simple that no recursion is necessary.

`go` returns a new constraint if necessary or fails with `Nothing`, this new constraint is then added to the substitution. `go` first checks if the two terms are equal, if they are no new constraint is added. If one of the terms is a variable add a new constraint and fail otherwise. The last clause is only reached if both terms are different atoms. Note that `go` only works because the terms have been pruned beforehand.

There is a subtle detail about the first clause of `go`. One might be tempted to only check whether two atoms are equal and leave the next clause to deal with variables. However, this would create cyclic variable chains when unifying a variable with itself.

With unification under our belts, we can define the predicates of our language.

    type Pred = (Subst, Int) -> [(Subst, Int)]

`(Subst, Int)` is the current substitution plus the index of the next free variable. A predicate maps the current state to any number of possible future states. Basically a predicate is a non-deterministic operation on some substitution. This, together with Haskell's laziness, effectively mimics backtracking.

The user isn't supposed directly manipulate variables, instead, a function for generating fresh variables is provided:

    fresh :: (Term -> Pred) -> Pred
    fresh f = \(s, i) -> f (Var i) (s, i + 1)

Unification is also wrapped in a predicate:

    (===) :: Term -> Term -> Pred
    t === t' = \(s, i) ->
        case unify s t t' of
            Just s  -> [(s, i)]
            Nothing -> []

The empty list represents failure: there are no possible future states for a failed unification.

Next comes the logical connectives for conjunction (and) and disjunction (or).

    conj :: Pred -> Pred -> Pred
    conj p p' = \s -> p s >>= p'

    disj :: Pred -> Pred -> Pred
    disj p p' = \s -> p s ++ p' s

[1]: http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf
[2]: https://github.com/seantalts/hasktrip/blob/master/doc/MicroKanren.md
[3]: https://hackage.haskell.org/package/unification-fd

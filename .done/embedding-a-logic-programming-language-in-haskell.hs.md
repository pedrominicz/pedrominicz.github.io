---
layout: default
permalink: /embedding-a-logic-programming-language-in-haskell
title: Embedding a logic programming language in Haskell
---

In this post we will implement μKanren, a minimal logic programming language, in Haskell. It's originally a Scheme domain-specific language introduced in [μKanren A Minimal Functional Core for Relational Programming][1].

Before proceeding any further, I want to recommend [this μKanren implementation walkthrough][2]. It is also written in Haskell and I will implement almost the same thing. So check it out if you want a different explanation or just want to read more.

Without further ado, let's begin.

    data Term
      = Var Int
      | Atom String
      deriving (Eq, Show)

Given how simple our terms are, unification is straight forward. But before defining unification, we first need to define substitutions.

    type Subst = [(Int, Term)]

And a function for pruning a chain of variables:

    -- Assumes no cyclic chains.
    prune :: Subst -> Term -> Term
    prune s (Var x) =
      case lookup x s of
        Just t  -> prune s t
        Nothing -> Var x
    prune _ t = t

The paper calls this function `walk`. I call it `prune` because it's equivalent to the `fullprune` function in the [`unification-fd` library][3].

Now we can define our unification function. Given a substitution and two terms, the function returns a new substitution under which the terms unify or fails and returns `Nothing`.

    unify :: Subst -> Term -> Term -> Maybe Subst
    unify s t1 t2 = (++ s) <$> go (prune s t1) (prune s t2)
      where
      go t1 t2 | t1 == t2 = Just []
      go (Var x) t = Just [(x, t)]
      go t (Var x) = Just [(x, t)]
      go _ _ = Nothing

Our terms are so simple that we don't need recursion!

`go` returns a new constraint or fails. It first checks if the two terms are equal. If they are, don't add a constraint. If one of them is a variable, add a constraint. We only reach the last clause if both terms are different atoms. Note that `go` only works because terms have been pruned beforehand.

There is a subtle detail about the first clause of `go`. It might be tempting to check only if two atoms are equal. However, this would create a cycle when unifying a variable with itself.

With unification under our belts, we can define the propositions of our language.

    type Prop = (Subst, Int) -> [(Subst, Int)]

`(Subst, Int)` is the current substitution plus the index of the next free variable. A proposition maps the current state to several states which satisfy it. It is a nondeterministic operation on some substitution. This, together with Haskell's laziness, effectively mimics backtracking.

The user isn't supposed directly manipulate variables. Instead, we provide a function for generating fresh variables.

    fresh :: (Term -> Prop) -> Prop
    fresh f = \(s, i) -> f (Var i) (s, i + 1)

We also wrap unification in a proposition.

    (===) :: Term -> Term -> Prop
    t1 === t2 = \(s, i) ->
        case unify s t1 t2 of
            Just s -> [(s, i)]
            Nothing -> []

An empty list represents failure: there are no states which satisfy a failed unification.

Next comes the logical connectives for conjunction (and) and disjunction (or).

    conj :: Prop -> Prop -> Prop
    conj p q = \s -> p s >>= q

    disj :: Prop -> Prop -> Prop
    disj p q = \s -> p s ++ q s

Conjunction uses `(>>=)` for the list monad. For lists, `(>>=)` is equivalent to `\x f -> concat $ map f x`, which is what we want. We apply the second proposition to all outputs of the first. If the first proposition fails, there is nothing to apply to the second, and it returns an empty list. If the second proposition fails for some input, it returns the empty list, which disappears after `concat`.

Disjunction is simpler. It just appends the output of the first and second propositions. Note that `(++)` is equivalent to a depth-first search, like in Prolog. If the first proposition returns an infinite list, the result of the second never is considered.

And that is it! We have implemented every feature of μKanren!

Here are a few utility functions used in the examples.

    true :: Prop
    true = \s -> [s]

    false :: Prop
    false = \_ -> []

    solve :: Prop -> [Subst]
    solve p = map fst $ p ([], 0)

`true` is trivially satisfied. `false` fails no matter what. And `solve` is just a convenience.

Given the simplicity of our language, it is hard to come up with elaborate examples. However, we have the "essence" of logic programming.

```
λ> a = Atom "a"
λ> b = Atom "b"
λ> solve $ fresh $ \x -> x === a
[[(0,Atom "a")]]
λ> solve $ fresh $ \x -> conj (x === a) (x === b)
[]
λ> solve $ fresh $ \x -> disj (x === a) (x === b)
[[(0,Atom "a")],[(0,Atom "b")]]
```

As expected, `x` can't be `a` _and_ `b`, but it can be `a` _or_ `b`.

```
λ> as x = disj (x === a) (as x)
λ> take 4 $ solve $ fresh $ \x -> as x
[[(0,Atom "a")],[(0,Atom "a")],[(0,Atom "a")],[(0,Atom "a")]]
```

We can make a recursive proposition by using a recursive function. `as` generates an infinite list of `a`s. Because of how we implemented disjunction, `x` will never unify with `b` in the following example:

```
λ> take 4 $ solve $ fresh $ \x -> disj (as x) (x === b)
[[(0,Atom "a")],[(0,Atom "a")],[(0,Atom "a")],[(0,Atom "a")]]
```

That is all I have for today. I hope to write more about unification, logic, and programming languages in the future!

[1]: http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf
[2]: https://github.com/seantalts/hasktrip/blob/master/doc/MicroKanren.md
[3]: https://hackage.haskell.org/package/unification-fd

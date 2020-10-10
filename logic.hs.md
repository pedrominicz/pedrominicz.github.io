---
permalink: /logic
layout: default
---

# Embedding a logic programming language in Haskell

One of the ideas behind this “blog” is to share cool things I find while studying. This post solely exists for me to have a chance to share one of my favorite pieces of code. We will be implementing μKanren, a minimal logic embedded programming language, in Haskell. Jason Hemann and Daniel P. Friedman first presented μKanren as a Scheme domain-specific language by in their paper [μKanren A Minimal Functional Core for Relational Programming][1].

Before proceeding any further, I want to recommend [Seal Talt’s μKanren implementation walkthrough][2]. We will be implementing almost the same thing, so feel free to follow his writeup instead.

Without further ado, we begin defining terms.

    data Term
      = Var Int
      | Atom String
      deriving (Eq, Show)

Unification is straight forward, given how simple our terms are. Before defining unification, however, we first need to define substitutions:

    type Subst = [(Int, Term)]

and a function for pruning a chain of variables:

    -- Assumes no cyclic chains.
    prune :: Subst -> Term -> Term
    prune s (Var v) =
      case lookup v s of
        Just t  -> prune s t
        Nothing -> Var v
    prune s t = t

The paper calls this function `walk`. I call it `prune` because it's equivalent to the `fullprune` function in the [`unification-fd` library][3].

Now we can define our unification function. Given a substitution and two terms, the function returns a new substitution under which the terms unify, if possible, and `Nothing` otherwise.

    unify :: Subst -> Term -> Term -> Maybe Subst
    unify s t t' = (++ s) <$> go (prune s t) (prune s t')
      where
      go t t' | t == t' = Just []
      go (Var v) t = Just [(v, t)]
      go t (Var v) = Just [(v, t)]
      go t t' = Nothing

Our terms are so simple that no recursion is needed!

`go` may return a new constraint or fail. It first checks if the two terms are equal; if they are, don't add a constraint. If one of them is a variable, add a constraint. We only reach the last clause if both terms are different atoms. Note that `go` only works because terms have been pruned beforehand.

There is a subtle detail about the first clause of `go`. It might be tempting to check only if two atoms are equal. However, this would create a cyclic variable chain when unifying a variable with itself.

With unification under our belts, we can define the propositions of our language.

    type Prop = (Subst, Int) -> [(Subst, Int)]

`(Subst, Int)` is the current substitution plus the index of the next free variable. A proposition maps the current state to several states which satisfy it. A proposition is a nondeterministic operation on some substitution. This, together with Haskell's laziness, effectively mimics backtracking.

The user isn't supposed directly manipulate variables, instead, a function for generating fresh variables is provided:

    fresh :: (Term -> Prop) -> Prop
    fresh f = \(s, i) -> f (Var i) (s, i + 1)

Unification also is wrapped in a proposition:

    (===) :: Term -> Term -> Prop
    t === t' = \(s, i) ->
        case unify s t t' of
            Just s  -> [(s, i)]
            Nothing -> []

An empty list represents failure: there are no possible states which satisfy a failed unification.

Next comes the logical connectives for conjunction (and) and disjunction (or).

    conj :: Prop -> Prop -> Prop
    conj p q = \s -> p s >>= q

    disj :: Prop -> Prop -> Prop
    disj p q = \s -> p s ++ q s

Conjunction uses `(>>=)` for the list monad. For lists, `(>>=)` is equivalent to `\x f -> concat $ map f x`, which is the behavior we want. We apply the second proposition to all outputs of the first. If the first proposition fails, there is nothing to apply to the second, and it returns an empty list. If the second proposition fails for some input, it returns the empty list, which disappears after `concat`.

Disjunction is simpler. It just appends the output of the first and second propositions. Note that `(++)` is equivalent to a depth-first search, like in Prolog. If the first proposition returns an infinite list, the result of the second never is considered.

And that is it! We have implemented every feature of μKanren!

We present a few utility functions before showing some examples.

    true :: Prop
    true = \s -> [s]

    false :: Prop
    false = \s -> []

    solve :: Prop -> [Subst]
    solve p = map fst $ p ([], 0)

`true` is trivially satisfiable, `false` fails no matter what, and `solve` makes things convenient.

---

Given the simplicity of our language, it is hard to come up with elaborate examples. We can, however, demonstrate what can be called the "essence" of logic programming.

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

As expected, `x` can't be `a` and `b` simultaneously, but it can be `a` _or_ `b`.

```
λ> as x = disj (x === a) (as x)
λ> take 4 $ solve $ fresh $ \x -> as x
[[(0,Atom "a")],[(0,Atom "a")],[(0,Atom "a")],[(0,Atom "a")]]
```

We can make a recursive proposition by using recursion in the host language (Haskell). `as` generates an infinite list of `a`s. Because of how we implemented disjunction, `x` will never unify with `b` in the following example:

```
λ> take 4 $ solve $ fresh $ \x -> disj (as x) (x === b)
[[(0,Atom "a")],[(0,Atom "a")],[(0,Atom "a")],[(0,Atom "a")]]
```

That is all I have for today. I hope to write more about [unification][4], logic, and programming languages in the future!

[1]: http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf
[2]: https://github.com/seantalts/hasktrip/blob/master/doc/MicroKanren.md
[3]: https://hackage.haskell.org/package/unification-fd
[4]: /unify

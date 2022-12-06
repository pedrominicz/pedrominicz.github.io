---
layout: post
permalink: /bracket-abstraction-and-the-s-combinator
title: Bracket abstraction and the S combinator
---

When I first studied combinatory logic, I thought the S combinator was very weird. I think I am not alone. But after I learned about bracket abstraction, its meaning became clear. Bracket abstraction is used when compiling lambda calculus into SKI combinator calculus. That is the best context to understand the S combinator.

In this post we will implement a lambda calculus to SKI combinator calculus compiler and get a deep understanding of the S combinator as well as the K, I, B and C combinators. I recommend trying it yourself before continuing. There is a chance you'll reinvent bracket abstraction.

As a quick reminder, here are the SKI combinators in Haskell:

```
s :: (a -> b -> c) -> (a -> b) -> a -> c
s x y z = x z (y z)

k :: a -> b -> a
k x y = x

i :: a -> a
i x = x
```

Okay, let's get started.

```Compile.hs
type Var = String

data SKI
  = S
  | K
  | I
  | V Var
  | A SKI SKI
  deriving (Eq, Show)

data Lam
  = Var Var
  | Lam Var Lam
  | App Lam Lam
  deriving (Eq, Show)
```

Our goal is to create a function `compile :: Lam -> SKI`. The cases for variables and applications is simple.

```
compile :: Lam -> SKI
compile (Var x) = V x
compile (App f a) = A (compile f) (compile a)
```

What about functions?

```
compile (Lam x b) = _
```

The only way to make progress is to compile the body of the function.

```
compile (Lam x b) = compile b
```

But that is not the term we want, `x` may be free in `compile b` and now we don't have the lambda to do substitution for us. We need to find a term that when applied to an argument behaves like substitution. We need to _abstract_ the variable `x` from the term `compile b`.

```
compile (Lam x b) = abstract x (compile b)
```

The simplest case is abstracting a variable from itself. Imagine we are compiling `λ x, x`. Intuitively it should result in the I combinator.

```
compile (λ x, x) = abstract x (compile x) = abstract x x = I
```

What about abstracting from a distinct variable? Note that `K y` behaves like `λ x, y` when applied to an argument.

```
compile (λ x, y) = abstract x (compile y) = abstract x y = K y
```

Mixing lambda terms and SKI terms, see that `λ x, S`, `λ x, K` and `λ x, I` behave like `K S`, `K K` and `K I`, respectively.

```
abstract :: Var -> SKI -> SKI
abstract _ S = A K S
abstract _ K = A K K
abstract _ I = A K I
abstract x (V y) =
  if x == y
    then I
    else V y
```

Now imagine we are compiling `λ x, f a`, `x` may be free in either `f` or `a`.

```
abstract x (A f a) =
  let f' = abstract x f
      a' = abstract x a in
  _
```

We need a term that behaves like `λ x, f' x (a' x)` when applied to an argument. Wait a minute, the body of that function looks familiar. It is the right hand side of the computation rule for the S combinator!

```
S f' a' x = f' x (a' x)
```

That is it! We implemented bracket abstraction. Specifically, we implemented the so-called algorithm A. It just one of many ways of doing bracket abstraction.

Tidying it up.

```Compile.hs
compile :: Lam -> SKI
compile (Var x) = V x
compile (Lam x b) = abstract x (compile b)
compile (App f a) = A (compile f) (compile a)

abstract :: Var -> SKI -> SKI
abstract x (V y) | x == y = I
abstract x (A f a) = A (A S (abstract x f)) (abstract x a)
abstract _ t = A K t
```

If we think of the variable we are abstracting over as some sort of environment, then the I combinator uses the environment, the K combinator discards the environment and the S combinator distributes the environment over function application. The S combinator is just a form of generalized function application!

Let's look at the B and C combinators.

```Compile.hs
b :: (b -> c) -> (a -> b) -> a -> c
b x y z = x (y z)

c :: (a -> b -> c) -> b -> a -> c
c x y z = x z y
```

It is common to think of B as function composition and C as `flip`. But now we have a new perspective. The B combinator generalizes function application with an environment that only applies to the argument and the C combinator generalizes function application with an environment that only applies to the function.

---

If you are interested in reading more, I recommend reading [Bracket abstraction algorithms][1]. It is a great tutorial with interactive examples of many bracket abstraction algorithms. I also recommend Oleg Kiselyov's paper [λ to SKI, Semantically][2]. It shows an effective algorithm for compiling lambda calculus to combinator calculus.

[1]: https://www.cantab.net/users/antoni.diller/brackets/intro.html
[2]: https://okmij.org/ftp/tagless-final/ski.pdf

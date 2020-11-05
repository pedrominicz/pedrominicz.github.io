---
permalink: /unify
layout: default
---

# Unification by Example

Unification is beautiful. It is computational primitive behind logical programming languages such as Prolog or [μKanren][1] and it is the magic behind Haskell's type inference. Unification is so incredibly versatile it feels like magic: it can spot the differences between objects and organically merge them.

<!--

Write about unification in early artificial intelligence.

-->

But unification may seem scary at first. Then, it can feel intuitive but mysterious. The goal of this post is to give an intuitive and precise explanation of unification. In the [next post][2], we implement unification in Haskell.

### When are Trees Equal?

Programmers are fond of trees. Most data structures can be seen as trees. Even more so if you use a language like Haskell, which doesn't allow circular pointer chains.

Fortunately, logicians are also fond of trees. Formulas may be seen as (abstract syntax) trees and Zermelo-Fraenkel set theory (secretly) is basically the study of well-founded trees.

Trees are important and so are the questions we can ask about them. One of the first, arguably most natural question is to ask is: when can two trees be considered equal?

What would you answer if a friendly neighborhood gardener asked if the follow trees can be considered equal?

![](/sorry.png)

Although not perfectly equal, the gardener defines these trees as equal: two branches, three leafs on the left branch, and no leafs on the right branch.

Then, the friendly gardener asks if the following trees are equal:

![](/sorry.png)

They are not, the gardener explains, because one has no leafs in its right branch while the other has one.

A programmer may see the trees above as a data structure containing natural numbers---the number of leafs---while a logician may see them as predicates.

![](/sorry.png)

### When Trees can be made Equal?

<!--

TODO:
- variables
- holes

-->

[1]: /logic
[2]: /fuse

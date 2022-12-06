This directory should contain work in progress blog posts. Completed posts should be moved to `.done`. Every post should be proofread.

### Todo

- Write more stuff!
- Make already visited links a different color

### Ideas

**Representable functors**

Write about monads which can by implemented in terms of `(<*>)` and `pure` ([representable functors][2]). This may be related with SKI combinator calculus and the reader monad (`((->) r)` is a `MonadReader r` with `ask = id`). How to describe `Applicative`, SKI combinator calculus, and `MonadReader` in categorical terms?

Interesting resources:
- [Applicative programming with effects][11] section 7
- [Applicative Functors][12]

### Low priority
- [An unsolvable problem of elementary number theory][3]
- [Deriving the Y combinator][4]
- [A timeline for logic, λ-calculus, and programming language theory][5]

[2]: https://funprog.zulipchat.com/#narrow/stream/201385-Haskell/topic/Arrow.20monad.20and.20SK.20calculus/near/212781408
[3]: https://www.ics.uci.edu/~lopes/teaching/inf212W12/readings/church.pdf
[4]: https://homes.cs.washington.edu/~sorawee/en/blog/2017/10-05-deriving-Y.html
[5]: http://fm.csl.sri.com/SSFT15/Timeline.pages.pdf
[7]: https://arxiv.org/pdf/0903.0340.pdf
[8]: http://www-cs-students.stanford.edu/~blynn/lambda/sk.html
[9]: https://doisinkidney.com/posts/2020-10-17-ski.html
[10]: http://okmij.org/ftp/tagless-final/ski.pdf
[11]: http://www.staff.city.ac.uk/~ross/papers/Applicative.pdf
[12]: https://bartoszmilewski.com/2017/02/06/applicative-functors/

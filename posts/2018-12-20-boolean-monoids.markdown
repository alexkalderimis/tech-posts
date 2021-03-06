---
title: 'Abusing Monoids: Monoidal predicate combinators'
draft: false
description: There are many ways to skin a predicate, and monoids fold into one of
             them.
tags: haskell, monoids
---

There is always another abstraction...
======================================

One of the nice things about the succinct ways we have in Haskell for writing
functions is that our predicates can read very naturally:

```haskell
filter (> 5) [0 .. 10]
```

We can compose a chain of functions that produce a `Bool` with normal function
composition: 

```haskell
filter ((== 'c') . fst) [('a', 1), ('b', 2), ('c', 3)]
```

But often you want to compose predicates logically, with _and_, _or_ and _not_,
for example:

```haskell
filter (\(k,v) -> elem k "ac" && v > 1) [('a', 1), ('b', 2), ('c', 3)]
```

And that is when things start to get ugly. While the syntax is lightweight, it
still gets noisy enough to get in the way of our meaning. Even more annoyingly,
when we decide to abstract the predicates we are composing, we don't gain any
clarity. Imagine the decision process for deciding if a patron:

```haskell
newtype Age          = Age Int deriving (Show, Eq, Ord)
newtype BloodAlcohol = MgMl Double deriving (Show, Eq, Ord)
data Person = Person { age :: Age, bloodAlcohol :: BloodAlcohol }
```

Can be served a drink in a bar:

```haskell
drinkingAge = Age 18
legalLimit  = MgMl 80.0

isOldEnough = (drinkingAge <=) . age
isDrunk     = (legalLimit  <=) . bloodAlcohol

customers = filter (\patron -> isOldEnough patron && not (isDrunk patron))
           [ Person (Age 17) (MgMl 0.0)  -- too young
           , Person (Age 22) (MgMl 93.5) -- too drunk
           , Person (Age 20) (MgMl 62.1) -- fine (for now)
           ]
```

That is ok, but having to thread the `patron` argument to the two predicates
and combine the results, well, it all feels a bit low level! Where is the
abstraction? We have abstracted out the predicates themselves, but we can't
abstract their composition. Can this not be more declarative? Perhaps our ideal
syntax would be:

```haskell
filter (isOldEnough <&> not' isDrunk) customers
```

So much nicer! And seriously, expressing intent like this means that logic is
clearer, less obscured by the ceremony of argument passing, and hopefully easier
to get right.

When all you have is functions, everything looks like composition
-------------------------------------------------------------------

Given we know the type of what we want to do, [hoogle][hoogle] is the obvious place to begin one's search. I remember searching in Hoogle for a
function of the appropriate type:

``` haskell
f :: (a -> b -> c) -> (d -> a) -> (d -> b) -> (d -> c)
```

so that I could create `(<&>)` as `f (&&)`, before realising
that this could be done in a couple of different ways.

The most obvious one (and the one that is used all the time in real code) is
with _Functors_ and _Applicatives_ (see below for a brief discussion), which is
natural given the 'fmappy' kind of thing we are trying to do here - we are trying to apply a function (in this case `(&&)`) under some other kind of object, in this case two functions.

Today if you search in Hoogle you there is an answer to this query
([apply2Way][apply2Way] - a function in an obscure and dated package called
`yjtools`), and it still doesn't suggest _Applicatives_, which is unfortunate.
So let's go ahead and find our own way to address this desire, without resorting
to _Applicatives_ at all. Perhaps surprisingly, _Monoids_ are a perfectly
suitable (if slightly round-about) way forwards. This is surprising because
_Applicatives_ and _Monoids_ have different kinds, `* -> *` vs `*`, and yet we
can encode this little boolean algebra using either abstraction.

When two (predicates) become one
-----------------------------------

For this, we need to remind ourselves that monoids let us combine two similar
things. Examples of this are concatenating two strings, unioning two sets,
adding two numbers. The monoid class has the following methods:

```haskell
class Monoid m where
  mappend :: m -> m -> m -- also called (<>), and technically belongs to Semigroup
  mempty :: m            -- gets us the empty element
```

We are trying to combine two similar things! Specifically, two predicates into a
third either by way of `(&&)` or `(||)`. We can be clear about this by naming
that idea:

```haskell
type Pred a = a -> Bool
```

Can we treat our `Pred` as a Monoid?  Yes! And the key is that along with the
obvious things, such as strings and sets and numbers, functions also form a
monoid, as long as they produce a monoid (here written as lambdas to make it
clear that we are taking and returning functions):

```haskell
instance Monoid b => Monoid (a -> b) where
  mempty      = \a -> mempty
  mappend f g = \a -> f a <> g a
```

The `mappend` instance is particularly interesting: it enables us to compose two
functions to produce a third, but not in serial as with `(.)`, but in parallel.

Unfortunately our `Pred a` is not a Monoid yet, because there is no (one) Monoid
instance for `Bool`. In fact there are two.  The second piece of the puzzle is
that because there are multiple monoid instances for some data types, their
different instances are associated with newtypes, to allow us to specify which
one we mean.

Numbers have `Sum` and `Product`, representing the monoids `(+, 0)` and `(*, 1)`
respectively. Objects with orderings (members of the _Ord_) have the semigroup
instances `Min` and `Max` (where `(<>)` is `min` and `max`) as well as _Monoid_
instances if they are _Bounded_. Bools have `All` and `Any`, representing the
monoids `(&&, True)` and `(||, False)`.  With this in mind we can create our
little predicate combinators:

```haskell
combine :: (Functor f, Monoid (f m)) => (a -> m) -> (m -> b) -> f a -> f a -> f b
combine into outof f g = fmap outof (fmap into f <> fmap into g)
```

The `combine` helper just encapsulates the common logic for mappending two `Pred
a`, by allowing the isomorphic functions that select the monoid to be passed in.
With this we get:

```haskell
(<&>), (<?>) :: Pred a -> Pred a -> Pred a
(<&>) = combine All getAll
(<?>) = combine Any getAny

not' :: Pred a -> Pred a
not' = (not .) -- helps us avoid brackets
```

and now we can compose complex conditions in their own terms, producing a simple
boolean DSL, capable of handling even complex nested conditions:

```haskell
canDrink :: Pred Person
canDrink = isTheBoss `<?>` (hasValidId `<&>` isOldEnough `<&>` not' isDrunk)
```

While the higher kinded typeclasses of _Functors_ and _Applicatives_ are a more
natural fit for functions, it is always satisfying to see that the same thing
can be approached from different directions.

Postscript: The Applicative Approach
===============

The (more obvious) Applicative solution:

Like with Monoids, it is helpful to remind ourselves of the Applicative and
Functor instances for `(->)`, the type of functions, here with the specialised
signatures added for clarity, and reminding ourselves that `(->)` is written in
infix notation, so that `(->) a b` and `a -> b` are equivalent:

```haskell
instance Functor ((->) a) where 
  fmap :: (b -> c) -> (a -> b) -> (a -> c)
  fmap f g = f . g

instance Applicative ((->) a) where
  pure :: b -> (a -> b)
  pure = const

  (<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c)
  f <*> g = \x -> f x (g x)
```

While we don't see our `(<&>)` here just yet, it is worth remembering that the
type of `pure (&&) <*> isOldEnough` would be:

```haskell
(Person -> (Bool -> Bool -> Bool)) -> (Person -> Bool) -> Person -> Bool -> Bool
```

and if we have a `Person -> Bool -> Bool`, we can use use `<*>` again on a
second predicate:

```haskell
let appAnd = pure (&&)
    isOlderAnd = appAnd <*> isOldEnough
 in isOlderAnd <*> (not . isDrunk)
```

or, more simply (and making use of `<$>`, the infix synonym for `fmap`:

``` haskell
(&&) <$> isOldEnough <*> (not . isDrunk)
```

Can I get a lift here?
------------------------

When all the infix operators start making your eyes bleed, we can use `liftA2`
from the _Applicative_ class, which does exactly this:

``` haskell
liftA2 (&&) isOldEnough (not . isDrunk)
```

If we look back at the fearsome type signature from before:

``` haskell
f :: (a -> b -> c) -> (d -> a) -> (d -> b) -> (d -> c)
```

It should hopefully jump out at us that the last three values are all of the same
shape, differing only in their final parameter - i.e. they are all forms of
`d -> ?`. This should have triggered our senses earlier, and helped us discover
this solution lurking in the _Functor/Applicative/Monad_ hierarchy, by noting that
we are trying to _lift_ the function `(a -> b -> c)` so that it operates on
`(d -> a)` and `(d -> b)` and `(d -> c)`, i.e. lifting it into the `(->) d` _Functor_.

which means (if we can be bothered considering how trivial this is), we could
simply have defined `(<&>) = liftA2 (&&)` in the very beginning.

[hoogle]: https://hoogle.haskell.org/
[apply2Way]: https://hackage.haskell.org/package/yjtools-0.9.18/docs/Data-Function-Tools.html#v:apply2way

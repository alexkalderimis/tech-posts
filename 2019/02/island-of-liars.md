---
title: Consistently Logical
published: false
description: Using functional programming to solve logical puzzles in code
tags: haskell, the-riddler, logic
---

In the recent [538 Riddler][1], a particular logical puzzle was posted which
involves determining who is telling the truth if you assume their claims must
all be consistent and that they meet an extra constraint that all liars are
older than all truth-tellers. I'm going to show how we can proceed to solve this
in code, and specifically using functional programming with Haskell.

Modelling the problem
========================

The first place to start with any problem is deciding how to represent the
information in the puzzle. Often the operations on these inputs flow naturally
from the structures that we define.

The problem takes the form of a set of claims about the ages of other
participants in the puzzle, where each claim states that the age of some person
_is_ a value, _is not_ a value, is _more than_ a value, or is _less than_ a
value. So lets represent the claims as a tuple of the operation and the value:

```haskell
type Claim = (Op, Int)
```

We need to decide what the `Op` is. The most straightforward approach is a
sum-type with a constructor for each of the four operations we need, so:

```haskell ignore
data Op = Is | Isnt | Gt | Lt deriving (Show, Eq)
```

However, when a claimant is lying, it is not that we gain _no_ information,
instead we can infer the inverse of the operator - for example if someone is
lying that _X is 17_ then that is equivalent to someone telling the truth that
_X is not 17_. The same goes for `>` and `<`, but with the caveat that these
require the `>=` and `<=` operators to convey the inverses.

```haskell
data Op = Is | Isnt | Gt | Gte | Lt | Lte deriving (Show, Eq, Ord)

invert :: Op -> Op
invert Is   = Isnt
invert Isnt = Is
invert Gt   = Lte
invert Gte  = Lt
invert Lt   = Gte
invert Lte  = Gt
```

Then we need to model the claimants themselves. For our purposes, each of the
islanders is identified by a single letter, so we will use `Char`s for our
islander-names:

```haskell
type Islander = Char
```

And a claimant is an islander along with the set of their claims.

```haskell
data Claimant = Claimant
  { claimantName   :: Islander
  , claimantClaims :: Set (Islander, Claim)
  } deriving (Show, Eq)
```

For example, if 'X says Y is more than 10 years old', we can encode this as:

```haskell ignore
Claimant 'X' (S.fromList [('Y', (Gt, 10))])
```

And this is all we need to encode the problem as specified in the 538
description, which, with a couple of helpers, we can do in such a way that
it appears pretty similar to the problem statement itself.

```haskell
-- the islanders in the problem, with their claims
islanders :: [Claimant]
islanders = ['a' `saysThat` ['b' ==> (Gt, 20)
                            ,'d' ==> (Gt, 16)
                            ]
            ,'b' `saysThat` ['c' ==> (Gt, 18)
                            ,'e' ==> (Lt, 20)
                            ]
            ,'c' `saysThat` ['d' ==> (Lt, 22)
                            ,'a' ==> (Is, 19)
                            ]
            ,'d' `saysThat` ['e' ==> (Isnt, 17)
                            ,'b' ==> (Is, 20)
                            ]
            ,'e' `saysThat` ['a' ==> (Gt, 21)
                            ,'c' ==> (Lt, 18)
                            ]
            ]
  where saysThat who cs = Claimant who (S.fromList cs)
        (==>) = (,)
```

What are you implying?
======================

Once we know what each islander claims to be true, how can we find out whether
their claims are mutually consistent. For example we can see that 'A' says that
'B is more than 20', but 'D' says that 'B is 20'. Clearly they cannot both be
right, but how would we determine that algorithmically?

Well, we can look at what each claim implies. Each claim is equivalent to saying
that a value lies in one or more range of values. For example `Gt 10` is the same
as saying 'in the range 11 ..', and `Is 12` is the same as 'in the range 12 ..
12'. We can model `Isnt` as saying that a value must lie in one of two ranges,
each of which excludes the value, so `Isnt 12` means 'must lie in the range
..11, or in the range 13..'.

This gives us a concept of the implications of each claim, and how we can
represent them:

```haskell
data InclusiveRange = Maybe Int :..: Maybe Int
  deriving (Show, Eq)

newtype Implication = OneOf { ranges :: NonEmpty InclusiveRange }
  -- deriving Semigroup makes 'or'-ing ranges easy
  deriving (Show, Eq, Semigroup)
```

We can then go ahead and write smart constructors for the different implications
of the various claims we can express:

```haskell
-- helper that makes these functions tidier
inRange :: InclusiveRange -> Implication
inRange = OneOf . pure

is :: Int -> Implication
is x = inRange $ pure x :..: pure x

isnt :: Int -> Implication
isnt x = lt x <> gt x

gt :: Int -> Implication
gt x = inRange $ pure (x + 1) :..: Nothing

gte :: Int -> Implication
gte x = inRange $ pure x :..: Nothing

lt :: Int -> Implication
lt x = inRange $ Nothing :..: pure (x - 1)

lte :: Int -> Implication
lte x = inRange $ Nothing :..: pure x
```

And we can combine these in a single function from a claim to its implications:

```haskell
implication :: Claim -> Implication
implication (op, age) = ($ age) $ case op of
  Is   -> is
  Isnt -> isnt
  Gt   -> gt
  Gte  -> gte
  Lt   -> lt
  Lte  -> lte
```

Making Inferences
==================

The next step is to look at how we can combine two implications to get the
logical consequent. For instance, from `Gt 10` and `Lt 15` we should be able to
infer that the value lies in the range `11..14`. This implies some kind of
monoidal structure, where implications can be combined to create more detailed
ones. In which case we need the `mempty` base case:

```haskell
anything :: Implication
anything = inRange $ Nothing :..: Nothing
```

And as a first pass we will look at what we can infer from two implications:

```haskell
-- apply a binary operation, taking lhs if rhs is Nothing, or rhs if
-- lhs is Nothing
safeBinOp :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
safeBinOp f ma mb = liftA2 f ma mb <|> ma <|> mb

infer :: Implication -> Implication -> Maybe Implication
infer (OneOf lhs) (OneOf rhs) = fmap OneOf . NE.nonEmpty . catMaybes
  $ liftA2 infer' (NE.toList lhs) (NE.toList rhs)
  where
    infer' (a :..: b) (a' :..: b') =
      let lb = safeBinOp max a a'
          ub = safeBinOp min b b'
       in if fromMaybe False (liftA2 (>) lb ub)
             then Nothing -- invalid, lb > ub
             else pure (lb :..: ub)
```

The inference from two implications is what we get by seeing what constraints
each range applies to each range on the other side. Any ranges that are invalid
as a result are removed by `catMaybes`, and if we don't get any ranges at all we
can't return any implication, which means that the two statements are in
contradiction.

Lets define a way to pretty print an implication, and then take this inference
mechanism for a spin!

```haskell
showImpl :: Implication -> String
showImpl = L.intercalate ";" . fmap showRange . NE.toList . ranges
  where
    showRange (Nothing :..: Just x) = ".." <> show x
    showRange (Just x :..: Nothing) = show x <> ".."
    showRange (Just x :..: Just y)  = if x == y
                                         then show x
                                         else show x <> ".." <> show y
    showRange _   = "anything"

-- Give a two-parameter kleisli arrow, apply it to two arguments
-- (hard to believe this isn't in Control.Monad tbh)
liftKleisli :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
liftKleisli f a b = liftA2 (,) a b >>= uncurry f
```

Now we can play around with `infer`:

```haskell ignore
λ> let p = putStrLn . maybe "Contradiction" showImpl
λ> let inferAll = foldr (liftKleisli infer) (pure anything) . fmap pure
λ> p $ inferAll [anything]
anything
λ> p $ inferAll [gt 5, lt 10]
6..9
λ> p $ inferAll [gt 5, lt 10, isnt 7]
6;8..9
λ> p $ inferAll [gt 5, lt 10, isnt 7, isnt 8]
6;9
λ> p $ inferAll [gt 5, lt 10, isnt 7, isnt 8, isnt 6]
9
λ> p $ inferAll [gt 5, lt 12, isnt 8, isnt 10]
6..7;9;11
λ> p $ inferAll [gt 20, is 20]
Contradiction
```

Great, we seem to have a working inference engine. We can now wrap that up in a
Monoid:

```haskell
newtype Consequence = Consequence { getConsequence :: Maybe Implication }
  deriving (Show, Eq)

instance Semigroup Consequence where
  (Consequence ma) <> (Consequence mb) = Consequence (liftKleisli infer ma mb)

instance Monoid Consequence where
  mempty = Consequence (pure anything)
  mappend = (<>)

consequence :: Implication -> Consequence
consequence = Consequence . pure
```

Finding a solution
====================

We now have a lot of the pieces we need to find a solution, and one way we can
go about that is to enumerate all the possible arrangements of liars and
truth-tellers on the island, checking to see which ones are internally consistent, 
and then checking that all the liars are older than the truth-tellers. In our
case that is perfectly tractable, as we only have to consider `2^5`, or 32,
arrangements.

First of all we need to tag an islander as either a liar or a truth-teller, and
while we can use a `Bool` for this, it is much better to use a specific
data-type for our purposes:

```haskell
data Honesty = Honest | Liar deriving (Eq, Show)
```

Then we need all 32 arrangements of these two values, eg:

* Honest, Honest, Honest, Honest, Honest
* Honest, Honest, Honest, Honest, Liar
* Honest, Honest, Honest, Liar, Honest
* and so on...

Lets create a small helper for that:

```haskell
-- produce an arrangement of xs of size n
arrangement :: [a] -> Int -> [[a]]
arrangement _  0 = [[]]
arrangement xs n = xs >>= \x -> (x:) <$> arrangement xs (n - 1)
```

And we can quickly check that this does what we want:

```haskell ignore
λ> length $ arrangement ['a', 'b'] 5
32
λ> take 3 $ arrangement ['a', 'b'] 5
["aaaaa","aaaab","aaaba"]
```

Now we can generate all 32 potential solutions that we need to consider:

```haskell
fmap (`zip` islanders) $ arrangement [Honest, Liar] (length islanders)
```

In order to find a solution, we need to combine all the claims about the
islanders together, potentially inverting them if they come from liars, and then
check to see if there were any contradictions, and whether it meets the age
rule. First, lets define the types of solutions and their conclusions:

```haskell
type Conclusions = Map Char Consequence
type Solution = [(Honesty, Claimant)]
```

We need a way to turn a solution into a conclusion about all its claims:

```haskell
conclusions :: [(Honesty, S.Set (Char, Claim))] -> Conclusions
conclusions solution =
  M.fromListWith (<>)
  . fmap (fmap (consequence . implication))
  $ [(who, (trust h op, val)) | (h, cs) <- solution
                              , (who, (op, val)) <- S.toList cs
    ]
  where trust Liar = invert
        trust Honest = id
```

and a way to know if a set of conclusions contains any contradictions:

```haskell
-- A set of conclusions is valid if there are no contradictions
viable :: Conclusions -> Bool
viable = all (isJust . getConsequence)
```

and a way to determine if it meets the age-limit rule, where we check that
the oldest honest person is younger than the youngest liar - or in the language
of ranges, that the highest lower bound on the age of the honest claimants is
lower than the lowest upper bound on the ages of the liars.

```haskell
meetsAgeLimitRule :: Solution -> Conclusions -> Bool
meetsAgeLimitRule claimants conc =
  let oldestHonest = getAge max lowerBound isHonest
      youngestLiar = getAge min upperBound (not . isHonest)
   in fromMaybe True (liftA2 (<) oldestHonest youngestLiar)
 where
   honest = S.fromList [who | (Honest, Claimant who _) <- claimants]
   isHonest = (`S.member` honest)

   lowerBound (OneOf cs) = let f (lb :..: _) = lb in safely min (f <$> cs)
   upperBound (OneOf cs) = let f (_ :..: ub) = ub in safely max (f <$> cs)

   getAge overall perPerson isRelevant
       = safely overall
       . fmap (getConsequence . snd >=> perPerson)
       . filter (isRelevant . fst)
       $ M.toList conc

-- safely apply a bin-op over a foldable of Maybes
safely :: (Foldable f, Ord a) => (a -> a -> a) -> f (Maybe a) -> Maybe a
safely f = foldr (safeBinOp f) Nothing
```

With that all in place, we can then proceed to search all the possible arrangements
and print out the viable ones we find:

```haskell
viables :: [(Solution, Conclusions)]
viables = filter (uncurry meetsAgeLimitRule)
        . filter (viable . snd)
        . fmap   (withConclusion . (`zip` islanders))
        $ arrangement [Honest, Liar] (length islanders)

withConclusion :: Solution -> (Solution, Conclusions)
withConclusion cs = (cs, conclusions (fmap claimantClaims <$> cs))

-- print out all the viable solutions
main :: IO ()
main = mapM_ printSolution (zip [1 ..] viables)
  where
    printSolution (n, (sol, conc)) = do
      putStrLn $ "Solution: " <> show n
      putStrLn $ replicate 15 '-'
      mapM_ (go conc) sol
      putStrLn ""
    go conc (h, c) = putStrLn
                   $ unwords [[claimantName c]
                             ,show h
                             ,maybe "" showImpl
                              (M.lookup (claimantName c) conc >>= getConsequence)
                             ]
```

TaDa!
=====

There is just one solution: 

```
Solution: 1
---------------
a Liar 19
b Liar 20
c Honest 18
d Honest ..16
e Liar 20..
```

And we can clearly see from the conclusions that this is internally consistent.
Walking through the puzzle manually with this solution should be enough to
convince you that the solution is sound, but to my mind the nicest part is how
naturally it flows from observing the way the parts are stuctured, from seeing
the idea of ranges just fall out of the claims, to the fact that we can just
fold the claims together using their monoidal structure and then being able to
read the conclusions back from the consequences we had calculated. Being able to
define the solution clearly made proceeding through the solution that much
easier.

[1]: https://fivethirtyeight.com/features/dont-trust-anyone-older-than-l/

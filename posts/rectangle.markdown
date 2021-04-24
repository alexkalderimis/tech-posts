---
title: Goldilocks Rectangle - finding a better solution 
draft: posts
published: 2019-02-27
description: Three different ways to solve a problem in a functional language,
             with some discussion of the pros and cons of each approach.
tags: haskell, algorithms, functional programming
numbersections: true
---

There are well known algorithms for many problems, but these are usually
presented in an imperative fashion, which can often be difficult to translate to
a functional language and immutable data-structures. Here we will look at three
ways to solve a classic problem, looking at what it means to implement different
solutions in a function style, using Haskell, and how that affects the code we
write. Like Goldilocks and her quest for the perfect porridge, I will suggest
that sometimes the ideal solution is actually a compromise between extremes.

![picture credit: openclipart.org][skyline]

# The problem

The classic problem here is finding the largest rectangular area under a
histogram, e.g.:

![Jkv, Public Domain, https://commons.wikimedia.org/w/index.php?curid=1886663][hist1]

In this histogram, which we might represent as the following sequence of bar
heights `[1,2,5,12,10,19,27,14,6,4,0,1]`, the greatest rectangular area is `50`, 
which is the area shown below:

![histogram with box][hist-with-box]

The reason that this is an interesting problem is that there are many ways to
approach it, with greatly varying performance characteristics, ranging from
_O(n²)_ through _O(n.log n)_ to _O(n)_, and they involve different approaches
and techniques to achieve.

For the purposes of the problem, the bars are assumed to all be 1 unit wide, and all
have a non-negative height.

# Preamble and imports

We will be making use of the following imports in general: 

``` haskell
import qualified Data.List as L
import Data.Maybe
```

Others will be discussed where required.

# Solution 1.

For the first solution, let's just think about taking a horizontal slice across
the histogram at different heights:

![histogram with slice at 12 units](/images/histogram-with-slice.svg)

If we slice across the histogram at the height of the 4th bar (12 units high),
we get two sections, one with 1 bar in it, and another with 3. We don't know at
this stage how far _up_ each bar extends, but we do know that they all extend
down towards 0. This means there are two rectangular regions of height 12 we
can draw, one of width 1, and one of width 3:

![histogram with regions of height 12](/images/histogram-with-slice-2.svg)

While we could consider every possible bar height, including values such as 11
which no bar is precisely, we really only need to consider the heights of the
bars themselves as places to make our slices. So we can proceed as follows:

For each bar:

* Slice the histogram at that height
* Multiply the size of each slice by the height of the bar

And then take the maximum value.

You can view an animated version of this on [observablehq](https://observablehq.com/@alexkalderimis/slicing-a-histogram).

Taking a slice across the histogram could look like this:

``` haskell
sliceAtHeight h = L.group . fmap (>= h)
```

Lets look at what we get when we slice our histogram at the height of 12:

```haskell
λ> let hist = [1,2,5,12,10,19,27,14,6,4,0,1]
λ> sliceAtHeight 12 hist
[[False,False,False],[True],[False],[True,True,True],[False,False,False,False]]
```

Great, we get either voids (groups of `False`) or intersections (groups of
`True`) taken as a cross section across the histogram. Now all we need to do is
ignore the voids and find the largest group, and then multiply the size of that
by the height:

```haskell
maxRectAt h = (h *) . maximum . fmap length . filter head . L.group . fmap (>= h)
```

Again, we can see what this tells us the height of 12:

```haskell
λ> maxRectAt 12 hist
36
```

And this leads us to our first definition:

```haskell
solution_1 :: [Int] -> Int
solution_1 xs = maximum (0 : [maxRectAt x xs | x <- xs])
```

There are a couple of things to observe here. One is that this approach uses
several partial functions (specifically `head` and `maximum`), and care must be
taken to ensure that they are safe. The use of head is safe, because
`Data.List.group` is guaranteed to never produce empty groups. If there are no
groups, then we get an empty set of groups instead. The use of maximum in
`maxRectAt` is safe because we are scanning the input list itself, which by
definition includes one bar at least which matches our criterion `(>= h)`. The
use of `maximum` in `solution_1` itself would error if we passed in an empty
list of bars, so we instead we ensure that the list passed to maximum will have
at least one value, `0`, at its head. A downside of this approach is that we
need to reassure ourselves of this safety, and the compiler cannot help us here.

Secondly it is nice to see how clean and clear this solution is. It uses
standard list processing functions such as `fmap`, `filter` and `group`, and
being able to express the rectangular area calculation in a single composed
expression is satisfying. 

There is a further optimisation here we can make use of - since two bars of the
same height will produce the same maximum, we can `nub` the input to reduce this
duplicated work - `nubOrd` from [containers][containers] is a great choice here:

```haskell
solution_1_nubbed :: [Int] -> Int
solution_1_nubbed xs = maximum (0 : [maxRectAt x xs | x <- nubOrd xs])
```

Unfortunately, while elegant, this is the naive _O(n²)_ solution (nubbing
notwithstanding). For each bar we consider, 
we scan the whole list to calculate the area at that height.

## A Small improvement

What about instead of scanning the whole list we just scanned the relevant bits,
i.e. the sections in-front of and behind the bar itself. We only need to look in
either direction until we find a bar lower than the current one. For this we can
make use of two little functions from `Data.List`: `inits` and `tails`. These
produce all the prefixes before an index and all the suffixes after the index.
We can imagine that `inits` can be defined as `[take n xs | n <- [0 .. length xs]]`, and `tails` can
be defined as `[drop n xs | n <- [0 .. length xs]]`. This means we will
generally have to scan much less of the input, and perhaps produce a more
efficient solution:

```haskell
initsAndTails xs =
  let count x = length . takeWhile (>= x)
      rs = zipWith count                     xs (L.tails xs)
      ls = zipWith (\x -> count x . reverse) xs (L.inits xs)
   in maximum $ 0 : zipWith (*) xs (zipWith (+) rs ls)
```

This will, for most realistic input, have much better amortised performance than
the naive solution, but it is technically still _O(n²)_, since for each input
value we need to scan the tail and the prefix, and in the pathological case
where every value is the same, then we still need to scan every value for every
input, and on top of that, we also need to need to reverse the list.

# Solution 2.

Let's look instead at a well-known _O(n)_ solution, that only considers each bar
once, and uses a stack to manage the look-ahead.

The idea here is to keep a stack of all the bars we have seen for which we have
not yet seen a matching end-bar. As soon as a section is terminated (by
encountering a bar that is smaller than the previous one) we can remove that
from the stack. This means that the stack is kept in ascending order, and the
item before the top of the stack represents the left-edge of the region.

You can see an animation of this algorithm [here][animations-stacking]

This algorithm is fundamentally stateful, requiring the concept of a stack of
bars (including their indices) as well as the concept of a current index, which
may not be the bar we are considering, especially when we come to pop the stack.
While we could use the `State` monad to model this, stateful loops can be expressed more simply with a tail-recursive function, which should compile
down to the same tight loop you would see in a more imperative language:

```haskell
type Index  = Int
type Height = Int
type Area   = Int
type Stack  = [(Index, Height)]

stacking :: [Height] -> Area
stacking = go 0 0 [] 
  where
    go :: Area -> Index -> Stack -> [Height] -> Area
    go mv i stack input
       | (x:xs) <- input , gtTopOfStack x stack
                   = go mv (i + 1) ((i,x):stack) xs
       | ((_, tv) : stack') <- stack
                   = let area = tv * width i stack'
                      in go (max mv area) i stack' input
       | otherwise =  mv
                       
    top            = listToMaybe
    gtTopOfStack x = maybe True ((x >=) . snd) . top
    width i stack  = maybe i (\j -> i - j - 1) (fst <$> top stack)
```

Here we have a single worker function, `go` which is called in tail-recursive
position so long as there are values in either the input or the stack. Like a
lot of such functions, keeping the types straight in your head is half the
difficulty, so here a signature has been given to the worker function that helps
identify what all the various `Int`s actually mean in this context. Being able
to give these meaningful names is more helpful to the poor human reader than
saying the stack is of type `[(Int, Int)]`.

### Mapping thought to action

We can see immediately that there are three cases:

1. There is a new unconsidered input value and it is at least the height
   of whatever is on top of the stack (which we consider true if the stack is empty)
1. The stack still has unprocessed values in it, which means that we have
   reached the right-hand-edge for whatever region the bar on the top of the stack
   is in. If the input list is empty, then we have reached the right-hand-edge
   for all the values remaining in the stack.
1. There are neither any new values nor any unprocessed values in the stack, so we
   have nothing to do, and we can return the largest value we have seen so far.

What immediately strikes me about this implementation is how clearly it accords
with the description of the algorithm - the algorithm has two conditions, and
our function has two bodies with pattern guards. We can see from a brief
inspection that at any point we are either advancing and stacking, or popping
and calculating, and that we alternate between the two. Also, this
implementation does not include any partial functions, and by making use of
lists as stacks, and pattern guards, the compiler keeps us honest about handling
the various cases.

What is less desirable here is how fiddly this is to get right. While this looks
tolerably clean here, translating an imperative procedure to a functional style
requires rethinking how it works. It can be satisfying though, and can even be said
that this is more concise and even intelligible than some of the standard
imperative presentations (especially since they tend to duplicate the handling
of the two conditions when we need to pop the stack).

I think it is fair to say though that this is not exactly an idiomatic
solution, and apart from its efficiency, I'm not sure it can exactly said to be
very nice to read. One of the problems with recursion is the cognitive burden
it places on the reader, and it has been compared to `goto` in terms of its
capacity to create cryptic code. The next solution will seek to strike a fair
balance between efficiency and clean code.

# Solution 3.

The final solution is rests on the observation that we can always make a
rectangular area of the height of the minimum value in the list of bars, with
the width equal to the length of the list of bars. This may not the largest
rectangle, but if it isn't that, then it must be taken solely from the bars to
the left or to the right of the minimum, which functions like a valley dividing
the two hills. And in each such hill, there is a similar rectangular area - i.e.
this is a recursive algorithm where we search through successively smaller
subsections of the input, considering the minimum value each time and splitting
the list at that point:

![histogram with three sections](/images/histogram-with-3-boxes.svg)

In the picture you can see a step in this process - either the maximum
rectangular area is the wide section at the bottom with the height of the
smallest bar, or it is contained within the two boxes left and right of that
bar. After calculating the height of the bar at the bottom, we can proceed
to apply the same process to the two sections to either side:

![divide and conquer](/images/histogram-divide-and-conquer.svg)

You can view an animated version of this on [observablehq](https://observablehq.com/@alexkalderimis/dividing-and-conquering)

These kinds of approaches translate very naturally to functional languages:

```haskell
divideAndConq = maxArea
  where
    maxArea = maybe 0 calc . splitAtMin
    calc (lhs, a, rhs) = maximum [ maxArea lhs
                                 , maxArea rhs
                                 , x * (length lhs + length rhs + 1)
                                 ]

splitAtMin :: Ord a => [a] -> Maybe ([a], a, [a])
splitAtMin [] = Nothing
splitAtMin xs = let m = minimum xs
                    (lhs, rhs) = span (/= m) xs
                 in Just (lhs, m, drop 1 rhs)
```

Nicer! This certainly looks a lot cleaner. Like the stack based solution, it is
clearly a translation into code of the description of the solution - with
handling of the left-hand-side, minimum value and right-hand-side of the
inputs. We have a partial function, but we are explicitly passing it three
values at a time, so that is not a problem.

What is more of an issue is actually the fact that most of our work is actually
going to be in the `splitAtMin` function, which needs to rescan the input each
time we recurse in order to find the new minimum. On the plus side we have
increasingly fewer items to scan to find the next minimum, but still, that is
going to involve a lot of repeated work, since finding the very first minimum
also implies finding the next ones. It would be nicer if we could perform all
those comparisons once and cache the results for later reuse. 

## Better performance through data-structures

One way to do this is to build up a search tree once that helps us quickly
locate the minimum value, while preserving the sequence. An example of such a
data structure is a so-called 'segment tree', where each node stores the minimum
value of all the nodes beneath it. Our tree will also store the size of the
trees below it, so that we can have _O(1)_ sizes. Happily, such trees are easy
to define in Haskell:

```haskell
data SegmentTree a
  = Empty
  | Tip a
  | Branch (Maybe a) Int (SegmentTree a) (SegmentTree a)
  deriving (Show, Eq)
```

The trick to making this work nicely is to consider what happens when be smush
two trees together to make a new composite tree. We want that new tree to
contain a summary of their minimum value and their total size, and the concept
of smushing together can be understood as the `(<>)` method of the `Semigroup`
type-class:

```haskell
instance (Ord a) => Semigroup (SegmentTree a) where
  Empty <> a = a
  a <> Empty = a
  a <> b = Branch (liftA2 min (value a) (value b) <|> value a <|> value b)
                  (size a + size b)
                  a b

instance (Ord a) => Monoid (SegmentTree a) where
  mempty = Empty
  mappend = (<>)
```

Here we discard empty trees when combining them with anything else, and from
tips and branches we build new tips, that in turn contain the global minimum
values and the sum of the sizes. So we need some way to get the value and the
size:

```haskell
value :: SegmentTree a -> Maybe a
value Empty          = Nothing
value (Tip a)        = Just a
value (Branch a _ _) = a

size :: SegmentTree a -> Int
size Empty            = 0
size Tip{}            = 1
size (Branch _ x _ _) = x
```

And being able to turn it back into a list would be nice:

```haskell
treeToList :: SegmentTree a -> [a]
treeToList = flip appEndo [] . go
  where go Empty            = Endo id
        go (Tip a)          = Endo (a:)
        go (Branch _ _ l r) = go l <> go r
```

As a next step, we need a way to build such a tree from a list of values. Since
trees are `Monoid`s, we can do this very simply with `foldMap Tip`, which wraps
each element in a Tip, and then `mappend`s them all together. However, this
happens to build very unbalanced trees. For example, if we build a tree in this
way, we can see it is right-biased:

```haskell
λ> foldMap Tip [1 .. 4]
Branch (Just 1) 4 (Tip 1)
                  (Branch (Just 2) 3 (Tip 2)
                                     (Branch (Just 3) 2 (Tip 3)
                                                        (Tip 4)))
```

Which builds the tree with tips on the left, and a long spine on the right:

```
         [min=1,len=4]
               |
      +--------+--------+
      |                 |
  Tip = 1         [min=2,len=3]
                        |
               +--------+--------+
               |                 |
           Tip = 2         [min=3,len=2]
                                 |
                        +--------+--------+
                    Tip = 3           Tip = 4
```

That's the same structure as a list! If the minimum is not at the head, we are
going to have to traverse a long section of the spine to find it, making it no
better than a list for random access (it is better when the minimum is near the
front of the list, since we still don't have to traverse the whole list to find
it). 

Since a balanced tree would be much nicer, in that it would produce more
consistent results, a quick way to do that is with the `treeFold` operation
(from Jon Fairbairn), which builds up the new folded structure without being
purely left or right associative:

```haskell
treeFold :: (a -> a -> a) -> a -> [a] -> a
treeFold f = go
  where
    go x [] = x
    go a  (b:l) = go (f a b) (pairMap l)
    pairMap (x:y:rest) = f x y : pairMap rest
    pairMap xs = xs
```

To see what this looks like, we can play around with this a bit:

``` haskell
λ> let nums = show <$> [0 .. 10]
λ> let f a b = concat ["(", a, ",", b, ")"]
λ> foldr f "" nums
"(0,(1,(2,(3,(4,(5,(6,(7,(8,(9,(10,)))))))))))"
λ> foldl f "" nums
"(((((((((((,0),1),2),3),4),5),6),7),8),9),10)"
λ> treeFold f "" nums
"((((,0),(1,2)),((3,4),(5,6))),((7,8),(9,10)))"
```

Much nicer than either `foldr` or `foldl`, and much more likely to give is the
`logn` search time we are going to be hoping for. Tree-fold cannot change the
type of its input (unlike `fold(r/l)`), since it doesn't know what direction it
is coming from, but that means it works well for anything that obeys the
Semigroup laws.

This allows us to build up a segment-tree easily from a list of inputs:

```haskell
minimalSearchTree :: Ord a => [a] -> SegmentTree a
minimalSearchTree = treeFold (<>) Empty . fmap Tip
```

And it produces much more balanced trees, which becomes clear as soon as we indent the printed
representation.

```haskell
λ> minimalSearchTree [1 .. 12]
Branch (Just 1) 12
       (Branch (Just 1) 7
               (Branch (Just 1) 3
                       (Tip 1)
                       (Branch (Just 2) 2
                               (Tip 2) (Tip 3)))
               (Branch (Just 4) 4
                       (Branch (Just 4) 2
                               (Tip 4) (Tip 5))
                       (Branch (Just 6) 2 (Tip 6) (Tip 7))))
       (Branch (Just 8) 5
               (Branch (Just 8) 4
                       (Branch (Just 8) 2
                               (Tip 8) (Tip 9))
                       (Branch (Just 10) 2
                               (Tip 10) (Tip 11)))
               (Tip 12))
```

All that is remaining here is to define a way to split the tree at its
(left-most) minimum point:

```haskell
splitTree :: Ord a
          => SegmentTree a -> Maybe (SegmentTree a, a, SegmentTree a)
splitTree = \case
  Empty          -> Nothing
  Tip a          -> pure (Empty, a, Empty)
  Branch v _ l r -> do let goLeft = v == value l
                       (lt, a, rt) <- splitTree (if goLeft then l else r)
                       pure $ if goLeft then (lt, a, rt <> r)
                                        else (l <> lt, a, rt)
```

The only real complexity here is deciding whether to descend into the LHS or the
RHS as we split the tree, depending on where the minimum is to be found. We also
need to remember to keep the other side around by associating it on the other
side as we return.

All these pieces let us write a divide-and-conquer function that performs the
minimum search once, and where each split takes _O(logn)_ time:

``` haskell
divideAndConq = maxArea . minimalSearchTree
  where
    maxArea t = flip (maybe 0) (splitTree t)
              $ \(lhs, x, rhs) -> maximum [maxArea lhs, maxArea rhs, x * size t]
```

Happily, the only thing we really need to change here is how we split the input,
and exchanging `length` for `size` - if we implement `Foldable` we can even use
`Foldable.length` and this becomes more generic.

And there we have it - a clear, declarative solution, that abstracts the
general portions of the implementation to a reusable data-structure rather than
burying it in the approach itself. You can see that the complexity is defined by
building up the search tree, then performing _N_ splits, which each take
 _O(logn)_ operations. We also get to benefit from _O(1)_ size operations
when calculating the base-case, which is nice.

## Summing it Up

While the stack based solution is the right approach if we absolutely must
optimise for speed, it suffers in terms of maintainability and clarity. It also
doesn't provide anything that can be re-used as is elsewhere, while the
tree-based solution leads us to an interesting general purpose data-structure.
_n.logn_ solutions are often more than good enough, and the benefit
from clarity seems to me to make this the Goldilocks option in this case.

# Post-Script(s)

## A generic divide and conquer

What if we want to support multiple search trees? We we could define that as
a typeclass. Since we 

``` haskell
class SearchTree t a where
  splitMin :: t a -> Maybe (t a, a, t a)
```

Then we can implement this interface for lists and `SegmentTree`s:

``` haskell
instance Ord a => SearchTree [] a where splitMin = splitAtMin

instance Ord a => SegmentTree SegmentTree a where splitMin = splitTree
```

This requires `MultiParamTypeClasses` and `FlexibleInstances` however, but these are fairly
standard extensions.

We are also going to need to make `SegmentTree`s instances of `Foldable`:

``` haskell
instance Foldable SegmentTree where
  foldMap f = \case Empty          -> mempty
                    Tip a          -> f a
                    Branch _ _ l r -> foldMap f l <> foldMap f r
```

Then we can define a generic version of `divideAndConq`:

``` haskell
divideAndConq :: (Ord a, Num a, Foldable t, SearchTree t a) => t a -> a
divideAndConq t = flip (maybe 0) (splitMin t)
                $ \(lhs, x, rhs) -> maximum [ divideAndConq lhs
                                            , x * fromIntegral (length t)
                                            , divideAndConq rhs
                                            ]
```

## Pulling your finger(-tree) out

Attentive readers (if there are any) may have noticed that the `SegmentTree`
presented here is just a special case of an even more general purpose
data-structure known as a `FingerTree`. Here we will reimplement a `SegmentTree`
in terms of a `FingerTree` and see how that compares:

First we will need to import it from the `fingertree` package:

```haskell
import           Data.FingerTree (ViewL(..), FingerTree, Measured(..))
import qualified Data.FingerTree as T
```

The primary concept of the `FingerTree` is the idea of a measure, which
associates the values with a datum, which is cached at the nodes. This value
must be a `Monoid` so that we can freely combine trees together. In our case
this is a combination of the minimum value and the sum of the number of items in
the tree. We need to define two _newtypes_, one for the measure, and one for the
value:

```haskell
newtype M a = M { um :: (Min a, Sum Int) }
   deriving (Show, Eq, Ord, Semigroup, Monoid)
newtype V a = V a
   deriving (Show, Eq, Ord)
```

Since the tuple of two monoids is a monoid, we can just derive all the instances
we need here. Next we need to define our `SegmentFingerTree`, which requires
associating `M`s and `V`s:

```haskell
instance (Ord a, Bounded a) => Measured (M a) (V a) where
  measure (V a) = M (Min a, Sum 1)

type SegmentFingerTree a = FingerTree (M a) (V a)
```

The measure for a value is just itself in a `Min` wrapper and the count of `1`,
and the `SegmentFingerTree` is just an alias for the underlying `FingerTree`.

We now need to implement the two fundamental operations - building the tree up
and tearing it down, and a function for getting the size of a tree.

```haskell
segmentFingerTree :: (Ord a, Bounded a) => [a] -> SegmentFingerTree a
segmentFingerTree = T.fromList . fmap V

splitFT :: (Ord a, Bounded a)
        => SegmentFingerTree a -> Maybe (SegmentFingerTree a, a, SegmentFingerTree a)
splitFT t = let mv         = fst . um $ measure t
                (lhs, rhs) = T.split ((== mv) . fst . um) t
             in case T.viewl rhs of
                  EmptyL -> Nothing
                  V a :< rhs' -> Just (lhs, a, rhs')

size :: (Ord a, Bounded a, Num b) => SegmentFingerTree a -> b
size = fromIntegral . getSum . snd . um . measure
```

Building a tree is just a thin wrapper around `fromList` itself that just wraps
each value in a `V` _newtype_. Splitting makes use of the fact that `measure`, when called
on the tree retrieves the cached value of that node, and we can then use the
`split` function to find the first place that minimum value is found. Otherwise
apart from a lot of _newtype_ wrapping/unwrapping, and the fact we don't need to do
any special bookkeeping, it is identical to the split we defined for `SegmentTree`.

Now we have all the pieces to define `divideAndConq` in terms of a finger-tree:

```haskell
divideAndConq2 :: (Ord a, Bounded a, Num a) => [a] -> a
divideAndConq2 = maxArea . segmentFingerTree
  where
    maxArea t = flip (maybe 0) (splitFT t)
              $ \(lhs, x, rhs) -> maximum [maxArea lhs, maxArea rhs, x * size t]
```

We can see that this is basically the same, but uses the more general
data-structure of the finger-tree underneath. If we compare what we had to
write to make this work, we can see we only needed to define the bits that
mattered to our domain - how to split at tree at the minimum value, how to get
the cached size. Building the tree itself, and all other operations such as the
ability to efficiently split and search it are gained simply by specifying
general operations around new-types, most of which the compiler generated for
us. This freed us up to focus on just the relevant sections of the code, which
is very small and clear indeed. The handwritten `SegmentTree` is more efficient
(as the benchmarks show), but this was much less work to write.

## Benchmarks

Talking about performance without numbers is a fool's errand. So we need to get
some numbers to back up our claims here. All the code for these implementations
(and a couple of other minor variations) is available [here][code], and an
example benchmark run is available [here][benchmark]. The benchmarks are not
super thorough, and are run on a cyclic input of either 1,000 elements, or
10,000 elements. The results are enlightening though:

| Approach                         | Complexity    | Small (1k) | Big (10k) | Big Sine |
| -------------------------------- | ------------- | ----------:| ---------:| --------:|
| naive                            | _O(n²)_       | 56.01 ms   | 6.033 s   | 5.646 s  |
| naive with nub                   | _O(n²)_       | 633.0 μs   | 6.768 ms  | 1.115 s  |
| inits-and-tails                  | _O(n²)_       | 5.808 ms   | 703.6 ms  | 770.8 ms |
| recursive - minimum with span    | _O(n²)    _   | 465.4 μs   | 25.80 ms  | 531.1 ms |
| recursive - custom tree, treemap | _O(n.logn)_   | 332.3 μs   | 7.296 ms  | 16.42 ms |
| recursive - custom tree, foldmap | _O(n.logn)_   | 320.1 μs   | 5.944 ms  | 1.829 s  |
| recursive - finger tree          | _O(n.logn)_   | 1.530 ms   | 19.00 ms  | 27.70 ms |
| stack                            | _O(n)_        | 30.89 μs   | 317.0 μs  | 334.2 μs |

There are a lot of take-aways from this set of benchmarks, and not all of it was
immediately obvious - at least not to me.

* Input matters

  The _big_ and _big sine_ inputs have the same number of data-points, but differ in their shape,
  wave-length and period. For some approaches this has very little effect (the naive approach,
  and the stack-based approach are not sensitive to this), but the other approaches are
  significantly affected, although to quite different degrees. Coping with typical and
  pathological/friendly input makes a big difference.

* The basic solution can be fast enough

  All the approaches are just fine for even moderately sized inputs - sweating the small
  performance gains is just not going to be worth it.

* Simple optimisation can produce massive wins

  Just nubbing the input makes a massive performance improvement for the naive approach. While this
  algorithm is still much slower than the others, for the speed of development, it remains
  pretty acceptable up to even large inputs - it is 1,000 times faster than the un-nubbed version,
  and completes the big data-set faster than some of the smarter approaches. This advantage
  disappears when faced with a less friendly data-set though.

* Choosing the right fold can make a world of difference

  When the next minimum is close to the left-hand-side of the data-set, `foldMap` works fine for
  producing a good search tree. However, when the data-set is less friendly, `treeMap` really
  shines, avoiding the spectacular blowup faced by the `foldMap`ed tree with the sine wave.

* Don't be afraid to reimplement

  Finger-trees are awesome general purpose data-structures, and are capable of implementing a
  wide variety of different things. They are seldom the best in class though - there are faster
  interval maps, faster heaps, and as you can see here, faster segment trees there to be used.
  Just as interesting is the small amount of code that was needed to produce something that
  out-performed finger-trees.

* You just can beat linear scans

  If you need the best, you just can't beat a low-level linear scan. But as good as it is, the
  divide-and-conquer approaches are more than fast enough for most people's needs, if you cannot
  find a linear scan that works for you.

# picture credits:

* open clipart
* Wikimedia commons

[treefold]: https://www.mail-archive.com/haskell@haskell.org/msg01788.html
[containers]: https://hackage.haskell.org/package/containers
[animations-stacking]: https://observablehq.com/@alexkalderimis/the-stack-based-solution

[skyline]: /images/skyline.png
[hist1]: /images/hist1.svg
[hist-with-box]: /images/histogram-box.svg





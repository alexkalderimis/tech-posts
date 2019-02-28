{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import qualified Data.List as L
import Data.Maybe
import Data.Ord
import Data.Monoid
import Data.Semigroup
import Control.Applicative
import Data.Foldable

import           Data.FingerTree (ViewL(..), FingerTree, Measured(..))
import qualified Data.FingerTree as T

bars :: [Int]
bars = [2,1,5,6,2,3]

naive xs = 
  let f n = (n *) . maximum . fmap length . filter head . L.group . fmap (>= n)
   in maximum (0 : [f x xs | x <- xs])

initsAndTails xs =
  let count x = length . takeWhile (>= x)
      rs = zipWith count xs (L.tails xs)
      ls = zipWith (\x -> count x . reverse) xs (L.inits xs)
   in maximum $ zipWith (*) xs (zipWith (+) rs ls)

divideAndConq' = go . splitAtMin
  where
    go = maybe 0 $ \(lhs, x, rhs) -> maximum [ go (splitAtMin lhs)
                                             , go (splitAtMin rhs)
                                             , x * (length lhs + length rhs + 1)
                                             ]

splitAtMin :: Ord a => [a] -> Maybe ([a], a, [a])
splitAtMin [] = Nothing
splitAtMin xs = let m = minimum xs
                    (lhs, rhs) = span (/= m) xs
                 in Just (lhs, m, drop 1 rhs)

divideAndConq = maxArea . minimalSearchTree
  where
    maxArea t = flip (maybe 0) (splitTree t)
              $ \(lhs, x, rhs) -> maximum [ maxArea lhs
                                          , maxArea rhs
                                          , x * size t
                                          ]

divideAndConq2 :: (Ord a, Bounded a, Num a) => [a] -> a
divideAndConq2 = maxArea . segmentFingerTree
  where
    maxArea t = flip (maybe 0) (splitFT t)
              $ \(lhs, x, rhs) -> maximum [maxArea lhs, maxArea rhs, x * sizeFt t]

sizeFt :: (Ord a, Bounded a, Num b) => SegmentFingerTree a -> b
sizeFt = fromIntegral . getSum . snd . um . measure . sft

segmentFingerTree :: (Ord a, Bounded a) => [a] -> SegmentFingerTree a
segmentFingerTree = SFT. T.fromList . fmap V

splitFT :: (Ord a, Bounded a)
        => SegmentFingerTree a -> Maybe (SegmentFingerTree a, a, SegmentFingerTree a)
splitFT t = let mv         = fst . um . measure $ sft t
                (lhs, rhs) = T.split ((== mv) . fst . um) (sft t)
             in case T.viewl rhs of
                  EmptyL -> Nothing
                  V a :< rhs' -> Just (SFT lhs, a, SFT rhs')

-- so we can measure
newtype M a = M { um :: (Min a, Sum Int) }
  deriving (Show, Eq, Ord, Semigroup, Monoid)

newtype V a = V a
   deriving (Show, Eq, Ord)

instance (Ord a, Bounded a) => Measured (M a) (V a) where
  measure (V a) = M (Min a, Sum 1)

newtype SegmentFingerTree a = SFT { sft :: FingerTree (M a) (V a) }
  deriving (Show, Eq, Ord, Semigroup, Monoid, Measured (M a))

type Index  = Int
type Height = Int
type Area   = Int

stacking :: [Int] -> Int
stacking = go 0 0 [] 
  where
    go :: Area -> Index -> [(Index, Height)] -> [Height] -> Area
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

data SegmentTree a = Empty
                   | Tip a
                   | Branch (Maybe a) Int (SegmentTree a) (SegmentTree a)
                   deriving (Show, Eq)

value :: SegmentTree a -> Maybe a
value Empty = Nothing
value (Tip a) = Just a
value (Branch a _ _ _) = a

size :: SegmentTree a -> Int
size Empty            = 0
size Tip{}            = 1
size (Branch _ x _ _) = x

treeToList :: SegmentTree a -> [a]
treeToList = flip appEndo [] . go
  where go Empty = Endo id
        go (Tip a) = Endo (a:)
        go (Branch _ _ l r) = go l <> go r

instance (Ord a) => Semigroup (SegmentTree a) where
  Empty <> a = a
  a <> Empty = a
  a <> b = Branch (liftA2 min (value a) (value b) <|> value a <|> value b)
                  (size a + size b)
                  a b

minimalSearchTree :: Ord a => [a] -> SegmentTree a
minimalSearchTree = treeFold (<>) Empty . fmap Tip

splitTree :: Ord a => SegmentTree a -> Maybe (SegmentTree a, a, SegmentTree a)
splitTree t = case t of
  Empty          -> Nothing
  Tip a          -> pure (Empty, a, Empty)
  Branch v _ l r -> do
    let splitLeft = v == value l
    (lt, a, rt) <- splitTree (if splitLeft then l else r)
    if splitLeft
      then pure (lt, a, rt <> r)
      else pure (l <> lt, a, rt)

treeFold :: (a -> a -> a) -> a -> [a] -> a
treeFold f = go
  where
    go x [] = x
    go a  (b:l) = go (f a b) (pairMap l)
    pairMap (x:y:rest) = f x y : pairMap rest
    pairMap xs = xs

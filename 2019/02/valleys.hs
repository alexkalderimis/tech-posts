{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}

import           Control.Applicative
import           Data.Monoid
import           Data.Ord
import           Data.Semigroup
import Data.List (foldl')
import Data.Foldable (Foldable(..))

import           Data.FingerTree     (FingerTree, Measured (..), ViewL (..))
import qualified Data.FingerTree     as T

import           Criterion.Main      (bench, bgroup, defaultMain, env, whnf)

setupEnv = do
  let bars  = cycle (profile <> basin <> basin2 <> basin3)
      k = 1000.0 :: Double
      -- a sine wave histogram, starting at 0 and rising to 2000, with a period of just under 1k
      sine  = floor . (+ k) . (* k)
            . sin . (+ (1.5 * pi)) . (/ (314.159 / 2))
            . fromIntegral <$> [(0 :: Int) ..]
  return (take 100000 bars, take 100000 sine)

main = defaultMain [
   env setupEnv $ \ ~(bars, sine) -> bgroup "main" [
   bgroup "tiny"  (group $ take    100 bars)
 , bgroup "small" (group $ take   1000 bars)
 , bgroup "big"   (group $ take  10000 bars)
 , bgroup "huge"  (group $ take 100000 bars)
 , bgroup "sine"  (group $ take  10000 sine)
 ] ]
 where
   group x =
     [ bench "dividing-segment-tree-treemap"  $ whnf (sum . valleys') x
     , bench "dividing-finger-tree"           $ whnf (sum . valleys) x
     , bench "dividing-split-tree"            $ whnf (sum . valleysSplitTree) x
     , bench "scanning"                       $ whnf (sum . doubleScan) x
     , bench "thereAndBack-tuples"            $ whnf thereAndBackTup x
     , bench "thereAndBack-strict-pairs"      $ whnf thereAndBack x
     ]


profile :: [Int]
profile = [1,3,4,2,7,3,6,5,3,2,4,1]
--                 x                          x
--                 x   x                      x ~ x
--                 x   x x                    x ~ x x      
--             x   x   x x     x   ->     x ~ x ~ x x ~ ~ x
--           x x   x x x x x   x        x x ~ x x x x x ~ x
--           x x x x x x x x x x        x x x x x x x x x x
--         x x x x x x x x x x x x    x x x x x x x x x x x x
--  Depth:                            0 0 0 2 0 3 0 0 1 2 0 0
--  Total water volume: 8

basin :: [Int]
basin   = [6,5,4,2,4,3,2,3,3,4,5,7]
--                               x                          x 
--         x                     x    x ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ x 
--         x x                 x x    x x ~ ~ ~ ~ ~ ~ ~ ~ x x 
--         x x x   x         x x x -> x x x ~ x ~ ~ ~ ~ x x x 
--         x x x   x x   x x x x x    x x x ~ x x ~ x x x x x 
--         x x x x x x x x x x x x    x x x x x x x x x x x x 
--         x x x x x x x x x x x x    x x x x x x x x x x x x 
--  Depth:                            0 1 2 4 2 3 4 3 3 2 1 0
--
--  Total water volume: 25
--
basin2 :: [Int]
basin2  = [2,5,6,5,4,3,2,3,4,3,5,1]
--                                                         
--             x                         x                 
--           x x x             x       x x x ~ ~ ~ ~ ~ ~ x 
--           x x x x       x   x   ->  x x x x ~ ~ ~ x ~ x 
--           x x x x x   x x x x       x x x x x ~ x x x x 
--         x x x x x x x x x x x     x x x x x x x x x x x 
--         x x x x x x x x x x x x   x x x x x x x x x x x x
--  Depth:                           0 0 0 0 1 2 3 2 1 2 0 0
--
--  Total water volume: 11

basin3 :: [Int]
basin3  = [6,5,4,2,1,2,5,3,4,3,1,2]
--                                                         
--         x                          x                      
--         x x         x              x x ~ ~ ~ ~ x          
--         x x x       x   x       -> x x x ~ ~ ~ x ~ x      
--         x x x       x x x x        x x x ~ ~ ~ x x x x    
--         x x x x   x x x x x   x    x x x x ~ x x x x x ~ x
--         x x x x x x x x x x x x    x x x x x x x x x x x x
--  Depth:                            0 0 1 3 4 3 0 1 0 0 1 0
--
--  Total water volume: 13

newtype Minish a = Minish (Maybe (Min a)) deriving (Show, Eq, Semigroup, Monoid)

newtype V a = V { value :: a } deriving (Show)

instance (Ord a) => Measured (Minish a) (V a) where
  measure = Minish . Just . Min . value

type SegmentTree a = FingerTree (Minish a) (V a)

data MaxTree a = Branch a (MaxTree a) (MaxTree a) | Leaf a
  deriving (Show, Eq)

instance Ord a => Semigroup (MaxTree a) where
  a <> b = Branch (max (maxOf a) (maxOf b)) a b

data SplitTree a = SplitTree (Maybe (SplitTree a)) (Maybe (SplitTree a)) a
  deriving (Show, Eq)

instance Ord a => Semigroup (SplitTree a) where
  a@(SplitTree la ra av) <> b@(SplitTree lb rb bv)
    | av >= bv  = SplitTree la (ra <> Just b) av
    | otherwise = SplitTree (Just a <> lb) rb bv

maxOf :: MaxTree a -> a
maxOf (Leaf a)       = a
maxOf (Branch a _ _) = a

instance Foldable MaxTree where
  foldMap f (Leaf a) = f a
  foldMap f (Branch _ l r) = foldMap f l <> foldMap f r

  toList = flip appEndo [] . go
    where go (Leaf a)       = Endo (a:)
          go (Branch _ a b) = go a <> go b

instance Foldable SplitTree where
  foldMap f (SplitTree lhs rhs a) = maybe mempty (foldMap f) lhs <> f a <> maybe mempty (foldMap f) rhs

  toList = flip appEndo [] . go
    where go (SplitTree lhs rhs a) = maybe mempty go lhs <> Endo (a :) <> maybe mempty go rhs

splitTree :: (Ord a) => MaxTree a -> (Maybe (MaxTree a), a, Maybe (MaxTree a))
splitTree (Leaf a) = (Nothing, a, Nothing)
splitTree (Branch mv l r) = (lpad <> lhs, mv, rhs <> rpad)
  where goLeft = mv == maxOf l
        (lhs, _, rhs) = splitTree (if goLeft then l else r)
        lpad = if goLeft then Nothing else Just l
        rpad = if goLeft then Just r else Nothing

splitFT :: (Ord a) => SegmentTree a -> Maybe (SegmentTree a, a, SegmentTree a)
splitFT t = let (lhs, view) = T.viewl <$> T.split (== measure t) t
           in case view of
                  EmptyL     -> Nothing
                  V a :< rhs -> Just (lhs, a, rhs)

valleys :: [Int] -> [Int]
valleys = flip appEndo [] . go 0 0 . T.fromList . fmap (V . Down)
  where
    go !a !b t = flip (maybe mempty) (splitFT t) $ \(lhs, Down x, rhs) ->
                   let d = depth a b x
                   in d `seq`  (go a (max x b) lhs
                                <> Endo (d :)
                                <> go (max x a) b rhs)

valleys' :: [Int] -> [Int]
valleys' = flip appEndo [] . go 0 0 . treeFold (<>) Nothing . fmap (Just . Leaf)
  where
    go !a !b = maybe mempty $ \t ->
      let (lhs, x, rhs) = splitTree t
       in go a (max x b) lhs <> Endo (depth a b x :) <> go (max x a) b rhs

valleysSplitTree :: [Int] -> [Int]
valleysSplitTree = flip appEndo [] . go 0 0 . treeFold (<>) Nothing . fmap leaf
  where
    leaf = Just . SplitTree Nothing Nothing
    go !a !b = maybe mempty $ \(SplitTree lhs rhs x) -> go a (max x b) lhs <> Endo (depth a b x :) <> go (max x a) b rhs

doubleScan :: (Ord b, Num b) => [b] -> [b]
doubleScan xs = zipWith3 depth (scanl max 0 xs)
                               (scanr max 0 xs)
                               xs

thereAndBackTup :: [Int] -> Int
thereAndBackTup = fst
   . foldl' (\(n,r) (x,l) -> (n + depth l r x, max x r)) (0,0)
   . foldl' (\stack x     -> (x, max x . snd $ head stack) : stack) [(0,0)]

thereAndBack :: [Int] -> Int
thereAndBack = pfst
   . foldl' (\(P n r) (P x l) -> P (n + depth l r x) (max x r)) (P 0 0)
   . foldl' (\stack x -> P x (max x . psnd $ head stack) : stack) [P 0 0]

-- a strict pair
data Pair a b = P { pfst :: !a, psnd :: !b }

depth :: (Ord a, Num a) => a -> a -> a -> a
depth l r x = max 0 $ min l r - x

treeFold :: (a -> a -> a) -> a -> [a] -> a
treeFold f = go
  where
    go x []     = x
    go a  (b:l) = go (f a b) (pairMap l)
    pairMap (x:y:rest) = f x y : pairMap rest
    pairMap xs         = xs

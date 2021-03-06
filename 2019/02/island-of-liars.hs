{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Control.Applicative
import           Control.Monad
import qualified Data.List           as L
import           Data.List.NonEmpty  (NonEmpty)
import qualified Data.List.NonEmpty  as NE
import qualified Data.Map.Strict     as M
import           Data.Maybe
import           Data.Semigroup
import qualified Data.Set            as S

type Map = M.Map
type Set = S.Set

data Honesty = Honest | Liar deriving (Eq, Show)

type Claim = (Op, Int)
type Islander = Char
data Op = Is | Isnt | Gt | Gte | Lt | Lte deriving (Show, Eq, Ord)

data InclusiveRange = Maybe Int :..: Maybe Int
  deriving (Show, Eq)

-- a claim about an age is either an inclusive range of ages (potentially
-- unbounded) or it is two or more disjoint ranges that represent the claim
-- that an age is either in range a or in range b
newtype Implication = OneOf { ranges :: NonEmpty InclusiveRange }
  deriving (Show, Eq, Semigroup)

newtype Consequence = Consequence { getConsequence :: Maybe Implication }
  deriving (Show, Eq)

instance Semigroup Consequence where
  (Consequence ma) <> (Consequence mb) = Consequence (liftKleisli infer ma mb)

instance Monoid Consequence where
  mempty = Consequence (pure anything)
  mappend = (<>)

consequence :: Implication -> Consequence
consequence = Consequence . pure

type Conclusions = Map Char Consequence
type Solution = [(Honesty, Claimant)]

data Claimant = Claimant
  { claimantName   :: Islander
  , claimantClaims :: Set (Islander, Claim)
  } deriving (Show, Eq)

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

-- pretty print a claim
showImpl :: Implication -> String
showImpl = L.intercalate ";" . fmap showRange . NE.toList . ranges
  where
    showRange (Nothing :..: Just x) = ".." <> show x
    showRange (Just x :..: Nothing) = show x <> ".."
    showRange (Just x :..: Just y)  = if x == y
                                         then show x
                                         else show x <> ".." <> show y
    showRange _   = "anything"

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

-- find all viable solutions
viables :: [(Solution, Conclusions)]
viables = filter (uncurry meetsAgeLimitRule)
        . filter (viable . snd)
        . fmap   (withConclusion . (`zip` islanders))
        $ arrangement [Honest, Liar] (length islanders)

-- produce an arrangement of xs of size n
arrangement :: [a] -> Int -> [[a]]
arrangement _  0 = [[]]
arrangement xs n = xs >>= \x -> (x:) <$> arrangement xs (n - 1)

-- Associate the conclusions with the solution. Rather than
-- recomputing the inferences every time we need them, we carry it
-- around with the solution.
withConclusion :: Solution -> (Solution, Conclusions)
withConclusion cs = (cs, conclusions (fmap claimantClaims <$> cs))

-- check that all liars are older than truth-tellers. To do this,
-- we find the ages that all truth-tellers must be, or be younger than, and
-- the ages that all liars must be or be older than, and then just compare them.
--
-- If one or other of the two sets does not have a limit, and liftA2 (<) returns
-- Nothing, then this meets the test, since one or other group is not constrained.
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

-- apply a binary operation, taking lhs if rhs is Nothing, or rhs if
-- lhs is Nothing
safeBinOp :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
safeBinOp f ma mb = liftA2 f ma mb <|> ma <|> mb

safely :: (Foldable f, Ord a) => (a -> a -> a) -> f (Maybe a) -> Maybe a
safely f = foldr (safeBinOp f) Nothing

-- A set of conclusions is valid if there are no contradictions
viable :: Conclusions -> Bool
viable = all (isJust . getConsequence)

-- Given a description of the claims, and whether they are honest,
-- gather the inferred conclusions
conclusions :: [(Honesty, S.Set (Char, Claim))] -> Conclusions
conclusions inp =
  M.fromListWith (<>)
  . fmap (fmap (consequence . implication))
  $ [(who, (trust h op, val)) | (h, cs) <- inp
                              , (who, (op, val)) <- S.toList cs
    ]
  where trust Liar = invert
        trust Honest = id

-- Give a two-parameter kleisli arrow, apply it to two arguments
-- (hard to believe this isn't in Control.Monad tbh)
liftKleisli :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
liftKleisli f a b = liftA2 (,) a b >>= uncurry f

-- If the claimant is lying, then the inversion of their claim must be
-- true. This defines the mapping.
-- The need to perform this mapping is the reason for the use of the Op
-- data-type, rather than using claims directly; it is much clearer and
-- more straightforward to do this, instead of inspecting a claim and
-- trying to re-infer what kind of claim it was.
invert :: Op -> Op
invert Is   = Isnt
invert Isnt = Is
invert Gt   = Lte
invert Gte  = Lt
invert Lt   = Gte
invert Lte  = Gt

-- Get the implication from a claim
implication :: Claim -> Implication
implication (op, age) = ($ age) $ case op of
  Is   -> is
  Isnt -> isnt
  Gt   -> gt
  Gte  -> gte
  Lt   -> lt
  Lte  -> lte

-- given two valid claims, infer the valid conclusion. If they are in
-- contradiction, then Nothing is returned.
infer :: Implication -> Implication -> Maybe Implication

-- where we have multiple disjoint possible ranges, we infer
-- by taking the cartesian product of the inferences.
infer (OneOf lhs) (OneOf rhs) = fmap OneOf . NE.nonEmpty . catMaybes
  $ liftA2 infer' (NE.toList lhs) (NE.toList rhs)
  where
    -- all claims have at least one defined bound, guaranteed by
    -- use of smart constructors, and a claim is valid so long
    -- as it can hold at least one value.
    infer' (a :..: b) (a' :..: b') =
      let lb = safeBinOp max a a'
          ub = safeBinOp min b b'
       in if fromMaybe False (liftA2 (>) lb ub)
             then Nothing -- invalid, lb > ub
             else pure (lb :..: ub)

-- smart constructors for implications
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

anything :: Implication
anything = inRange $ Nothing :..: Nothing

inRange :: InclusiveRange -> Implication
inRange = OneOf . pure

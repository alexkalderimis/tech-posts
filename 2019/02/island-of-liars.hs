import           Control.Applicative
import           Control.Monad
import qualified Data.List           as L
import qualified Data.Map.Strict     as M
import           Data.Maybe
import qualified Data.Set            as S

type Map = M.Map

data Honesty = Honest | Liar deriving (Eq, Show)

data Op = Is | Isnt | Gt | Gte | Lt | Lte deriving (Show, Eq)

-- a claim about an age is either an inclusive range of ages (potentially
-- unbounded) or it is two or more disjoint ranges that represent the claim
-- that an age is either in range a or in range b
data Claim
  = Claim (Maybe Int) (Maybe Int)
  | OneOf [Claim]
  deriving (Show, Eq)

type Conclusions = Map Char (Maybe Claim)
type Solution = [(Honesty, Claimant)]

data Claimant = Claimant
  { claimantName   :: Char
  , claimantClaims :: Map Char (Op, Int)
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
                             ,maybe "" showClaim 
                              (join $ M.lookup (claimantName c) conc)
                             ]

-- pretty print a claim
showClaim :: Claim -> String
showClaim (OneOf cs)                = L.intercalate ";" (fmap showClaim cs)
showClaim (Claim Nothing (Just x))  = ".." <> show x
showClaim (Claim (Just x) Nothing)  = show x <> ".."
showClaim (Claim (Just x) (Just y)) = if x == y
                                         then show x
                                         else show x <> ".." <> show y
showClaim (Claim Nothing Nothing)   = "Contradiction"

-- the islanders in the problem, with their claims
islanders :: [Claimant]
islanders = [Claimant 'a' (M.fromList [('b', (Gt, 20))
                                      ,('d', (Gt, 16))
                                      ])
            ,Claimant 'b' (M.fromList [('c', (Gt, 18))
                                      ,('e', (Lt, 20))
                                      ])
            ,Claimant 'c' (M.fromList [('d', (Lt, 22))
                                      ,('a', (Is, 19))
                                      ])
            ,Claimant 'd' (M.fromList [('e', (Isnt, 17))
                                      ,('b', (Is, 20))
                                      ])
            ,Claimant 'e' (M.fromList [('a', (Gt, 21))
                                      ,('c', (Lt, 18))
                                      ])
            ]

-- find all viable solutions
viables :: [(Solution, Conclusions)]
viables = filter (uncurry meetsAgeLimitRule)
        . filter (viable . snd)
        . fmap   (withConclusion . (`zip` islanders))
        $ arrangement [Honest, Liar] (length islanders)

-- produce an arrangement of xs of size n
arrangement :: [a] -> Int -> [[a]]
arrangement _ 0 = [[]]
arrangement xs n = xs >>= \x -> fmap (x:) (arrangement xs (n - 1))

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
  let honest = S.fromList [who | (Honest, Claimant who _) <- claimants]
      isHonest = (`S.member` honest)
      oldestHonest = highestMinAge isHonest conc
      youngestLiar = lowestMaxAge (not . isHonest) conc
   in fromMaybe True (liftA2 (<) oldestHonest youngestLiar)

-- find the highest minimum age in the subset of the conclusions selected
-- by the predicate. i.e. the age all islanders who match the predicate are
-- guaranteed to be <=
highestMinAge :: (Char -> Bool) -> Conclusions -> Maybe Int
highestMinAge isRelevant = safeMax
                         . fmap (snd >=> lowerBound)
                         . filter (isRelevant . fst)
                         . M.toList

-- find the lowest maximum age in the subset of the conclusions selected
-- by the predicate. i.e. the age all islanders who match the predicate are
-- guaranteed to be >=
lowestMaxAge :: (Char -> Bool) -> Conclusions -> Maybe Int
lowestMaxAge isRelevant = safeMin
                        . fmap (snd >=> upperBound)
                        . filter (isRelevant . fst)
                        . M.toList

-- apply a binary operation, taking lhs if rhs is Nothing, or rhs if
-- lhs is Nothing
safeBinOp :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
safeBinOp f ma mb = liftA2 f ma mb <|> ma <|> mb

safeMin :: Ord a => [Maybe a] -> Maybe a
safeMin = foldr (safeBinOp min) Nothing

safeMax :: Ord a => [Maybe a] -> Maybe a
safeMax = foldr (safeBinOp max) Nothing

lowerBound :: Claim -> Maybe Int
lowerBound (Claim minAge _) = minAge
lowerBound (OneOf cs) = safeMin (lowerBound <$> cs)

upperBound :: Claim -> Maybe Int
upperBound (Claim _ maxAge) = maxAge
upperBound (OneOf cs) = safeMax (upperBound <$> cs)

-- A set of conclusions is valid if there are no contradictions
viable :: Conclusions -> Bool
viable = all isJust

-- Given a description of the claims, and whether they are honest,
-- gather the inferred conclusions
conclusions :: [(Honesty, Map Char (Op, Int))] -> Conclusions
conclusions inp = M.fromListWith (liftKleisli infer)
                                 (fmap (pure . uncurry fromOp) <$> claims)
  where
    claims = [(who, (f op, val)) | (h, m) <- inp
                                 , (who, (op, val)) <- M.toList m
                                 , let f = if h == Liar then invert else id
             ]

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

-- Given a defunctionalised Op, get the function from age to claim
fromOp :: Op -> Int -> Claim
fromOp op = case op of
  Is   -> is
  Isnt -> isnt
  Gt   -> gt
  Gte  -> gte
  Lt   -> lt
  Lte  -> lte

-- given two valid claims, infer the valid conclusion. If they are in
-- contradiction, then Nothing is returned.
infer :: Claim -> Claim -> Maybe Claim

-- all claims have at least one defined bound, guaranteed by
-- use of smart constructors, and a claim is valid so long
-- as it can hold at least one value.
infer (Claim a b) (Claim a' b') =
  let lb = safeBinOp max a a'
      ub = safeBinOp min b b'
   in if fromMaybe False (liftA2 (>) lb ub)
         then Nothing -- invalid, lb > ub
         else pure (Claim lb ub)

-- where we have multiple disjoint possible ranges, we infer
-- by taking the cartesian product of the inferences.
infer (OneOf lhs) (OneOf rhs) =
  case catMaybes [infer a b | a <- lhs, b <- rhs] of
    []  -> Nothing
    [x] -> Just x
    xs  -> Just (OneOf xs)

-- dispatch to the second equation
infer a@OneOf{} b@Claim{} = infer a (OneOf [b])
infer a@Claim{} b@OneOf{} = infer (OneOf [a]) a

-- smart constructors for claims
is :: Int -> Claim
is x = Claim (pure x) (pure x)

isnt :: Int -> Claim
isnt x = OneOf [lt x, gt x]

gt :: Int -> Claim
gt x = Claim (pure (x + 1)) Nothing

gte :: Int -> Claim
gte x = Claim (pure x) Nothing

lt :: Int -> Claim
lt x = Claim Nothing (pure (x - 1))

lte :: Int -> Claim
lte x = Claim Nothing (pure x)

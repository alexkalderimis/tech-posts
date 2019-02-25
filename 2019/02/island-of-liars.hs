import           Control.Applicative
import           Control.Monad
import qualified Data.List           as L
import qualified Data.Map.Strict     as M
import           Data.Maybe
import qualified Data.Set            as S

type Map = M.Map

data Honesty = Honest | Liar deriving (Eq, Show)

data Op = Is | Isnt | Gt | Gte | Lt | Lte
           deriving (Show, Eq)

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

main :: IO ()
main = mapM_ printSolution (zip [1 ..] viables)
  where
    printSolution (n, (sol, conc)) = do
      putStrLn $ "Solution: " <> show n
      mapM_ (go conc) sol
      putStrLn ""
    go conc (h, c) = putStrLn
                   $ unwords [[claimantName c]
                             ,show h
                             ,maybe "" showClaim 
                              (join $ M.lookup (claimantName c) conc)
                             ]

showClaim :: Claim -> String
showClaim (OneOf cs)                = L.intercalate ";" (fmap showClaim cs)
showClaim (Claim Nothing (Just x))  = ".." <> show x
showClaim (Claim (Just x) Nothing)  = show x <> ".."
showClaim (Claim (Just x) (Just y)) = if x == y
                                         then show x
                                         else show x <> ".." <> show y
showClaim (Claim Nothing Nothing)   = "???"

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

viables :: [(Solution, Conclusions)]
viables = filter (uncurry meetsAgeLimitRule)
        . filter (viable . snd)
        . fmap (withConclusion . (`zip` islanders))
        $ [[a,b,c,d,e] | a <- [Honest, Liar]
                                    , b <- [Honest, Liar]
                                    , c <- [Honest, Liar]
                                    , d <- [Honest, Liar]
                                    , e <- [Honest, Liar]
          ]

withConclusion :: Solution -> (Solution, Conclusions)
withConclusion cs = (cs, conclusions (fmap claimantClaims <$> cs))

meetsAgeLimitRule :: Solution -> Conclusions -> Bool
meetsAgeLimitRule claimants conc =
  let honest = S.fromList [who | (Honest, Claimant who _) <- claimants]
      oldestHonest = ageLimit max (`S.member` honest) conc
      youngestLiar = ageLimit min (not . flip S.member honest) conc
   in case oldestHonest <|> fmap pred youngestLiar of
        Nothing  -> False
        Just lim -> viable (applyAgeLimit honest lim conc)

ageLimit :: (Int -> Int -> Int) -> (Char -> Bool) -> Conclusions -> Maybe Int
ageLimit f isRelevant concs =
  let knownAges = fmap (snd >=> knownAge)
                . filter (isRelevant . fst)
                . M.toList
                $ concs
   in foldr (\ma -> maybe ma (\b -> fmap (f b) ma)) Nothing knownAges

applyAgeLimit :: S.Set Char -> Int -> Conclusions -> Conclusions
applyAgeLimit honest limit = M.mapWithKey $ \who ->
  let constraint = if S.member who honest then lte else gt
   in (>>= infer (constraint limit))

knownAge :: Claim -> Maybe Int
knownAge (Claim (Just x) (Just y)) | x == y = Just x
knownAge _                         = Nothing

viable :: Conclusions -> Bool
viable = all isJust

conclusions :: [(Honesty, Map Char (Op, Int))] -> Conclusions
conclusions inp = M.fromListWith (liftKleisli infer) (fmap (fmap pure) claims)
  where
    claims = [(who, fromOp op val) | (Honest, m) <- inp
                                     , (who, (op, val)) <- M.toList m
             ]
             <>
             [(who, fromOp (invert op) val) | (Liar, m) <- inp
                                            , (who, (op, val)) <- M.toList m
             ]


liftKleisli :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
liftKleisli f a b = liftA2 (,) a b >>= uncurry f

invert :: Op -> Op
invert Is   = Isnt
invert Isnt = Is
invert Gt   = Lte
invert Gte  = Lt
invert Lt   = Gte
invert Lte  = Gt

fromOp :: Op -> Int -> Claim
fromOp op = case op of
  Is   -> is
  Isnt -> isnt
  Gt   -> gt
  Gte  -> gte
  Lt   -> lt
  Lte  -> lte

infer :: Claim -> Claim -> Maybe Claim

infer (Claim a b) (Claim a' b') =
  let lb = (max <$> a <*> a') <|> a <|> a'
      ub = (min <$> b <*> b') <|> b <|> b'
   in case (lb, ub) of
        (Nothing, Nothing) -> Nothing
        (Just x, Just y)   | x > y -> Nothing
        _                  -> Just (Claim lb ub)

infer (OneOf lhs) (OneOf rhs) =
  case catMaybes [infer a b | a <- lhs, b <- rhs] of
    []  -> Nothing
    [x] -> Just x
    xs  -> Just (OneOf xs)

infer a@OneOf{} b@Claim{} = infer a (OneOf [b])

infer a@Claim{} b@OneOf{} = infer (OneOf [a]) a


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

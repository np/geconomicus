{-# LANGUAGE TemplateHaskell, FlexibleContexts, RankNTypes, GADTs, ConstraintKinds, LambdaCase #-}
import Control.Applicative
import Control.Lens
import Control.Monad.Logic
import Control.Monad.State
import Control.Monad.Writer
import Data.Ord
import Data.Bitraversable
import Data.Map as Map (Map, keys, empty, fromList, partition, toList, null)
import Data.List as List
import Data.Monoid
import Debug.Trace

type GetterMonoid s a =
  forall f. (Contravariant f, Functor f, Monoid (f s)) =>
    (a -> f a) -> s -> f s

type FoldMonoid s a =
  forall f. (Contravariant f, Applicative f, Monoid (f s)) =>
    (a -> f a) -> s -> f s

(<<>>) :: Monoid a => Getting a s a -> Getting a s a -> Getter s a
x <<>> y = to $ \s -> (s ^. x) <> (s ^. y)

infixr 6 <<>>

showed :: (Profunctor p, Contravariant f, Functor f, Show s)
       => Optic' p f s String
showed = to show

const2 :: a -> b -> c -> a
const2 a _ _ = a

-- diff :: Num n => n -> n -> n
-- diff a b = abs (a - b)
diff :: (Eq n, Num n) => n -> n -> n
diff a b | a == b    = 0
         | otherwise = 1

nil :: Monoid a => a
nil = mempty

showMMap :: Show a => MMap a -> String
showMMap = unwords . map f . Map.toList
  where
    f (a, n) = concat [show n, show a]

at' :: (At m, Eq n, n ~ IxValue m) => n -> Index m -> Lens' m n
at' n k = at k . non n

at0 :: (At m, Eq n, Num n, n ~ IxValue m) => Index m -> Lens' m n
at0 = at' 0

atNil :: (At m, Eq n, Monoid n, n ~ IxValue m) => Index m -> Lens' m n
atNil = at' nil

{-
  This ASSUMES non empty lists!
-}
pick :: MonadState ([a], Random) m => m (Maybe a)
pick = do
  (as, r : rs) <- get
  if List.null as then pure Nothing
  else do
    let (bs, c : ds) = splitAt (r `mod` length as) as
    put (bs ++ ds, rs)
    pure $ Just c

delta :: a -> (a, a)
delta = \a -> (a, a)

-- Based on https://github.com/ekmett/lens/issues/315
mergeNonOverlappingOptics
    :: LensLike ((,) a) s t a a
    -- ^ Lens s t a a
    -> LensLike ((,) b) t u b b
    -- ^ Lens t u b b
    -> Lens s u (a, b) (a, b)
mergeNonOverlappingOptics la lb f s =
    let (a, t) = la delta s
        (b, _) = lb delta t
    in (\(a, b) ->
            let (_, t) = la (\a_ -> (a_, a)) s
                (_, u) = lb (\b_ -> (b_, b)) t
            in u
       ) <$> f (a, b)

-- Based on https://github.com/ekmett/lens/issues/315
merge3NonOverlappingOptics
    :: LensLike ((,) a) s t a a
    -- ^ Lens s t a a
    -> LensLike ((,) b) t u b b
    -- ^ Lens t u b b
    -> LensLike ((,) c) u v c c
    -- ^ Lens u v c c
    -> Lens s v (a, b, c) (a, b, c)
merge3NonOverlappingOptics la lb lc f s =
    let (a, t) = la delta s
        (b, u) = lb delta t
        (c, _) = lc delta u
    in (\(a, b, c) ->
            let (_, t) = la (\a_ -> (a_, a)) s
                (_, u) = lb (\b_ -> (b_, b)) t
                (_, v) = lc (\c_ -> (c_, c)) u
            in v
       ) <$> f (a, b, c)

-- Based on https://github.com/ekmett/lens/issues/315
merge4NonOverlappingOptics
    :: LensLike ((,) a) s t a a
    -- ^ Lens s t a a
    -> LensLike ((,) b) t u b b
    -- ^ Lens t u b b
    -> LensLike ((,) c) u v c c
    -- ^ Lens u v c c
    -> LensLike ((,) d) v w d d
    -- ^ Lens u v d d
    -> Lens s w (a, b, c, d) (a, b, c, d)
merge4NonOverlappingOptics la lb lc ld f s =
    let (a, t) = la delta s
        (b, u) = lb delta t
        (c, v) = lc delta u
        (d, _) = ld delta v
    in (\(a, b, c, d) ->
            let (_, t) = la (\a_ -> (a_, a)) s
                (_, u) = lb (\b_ -> (b_, b)) t
                (_, v) = lc (\c_ -> (c_, c)) u
                (_, w) = ld (\d_ -> (d_, d)) v
            in w
       ) <$> f (a, b, c, d)

--mergeSetters :: ASetter s t a b -> ASetter t u a b -> Setter s u a b
--mergeSetters l0 l1 = sets $ \f -> over l1 f . over l0 f

zoom'
  :: MonadState s m
  => LensLike' (Zoomed (StateT s m) a) s t -> StateT t m a -> m a
zoom' l m = do
  s <- get
  (r, s') <- runStateT (zoom l m) s
  put s'
  pure r

newtype Unit = Unit { _unitName :: String }
  deriving (Eq, Ord)

newtype ValueKind = ValueKind { _valueKindName :: String }
  deriving (Eq, Ord)

newtype PlayerName = PlayerName { _unPlayerName :: String }
  deriving (Eq, Ord)

data Value = Value
  { _valueKind :: ValueKind
  , _valueRank :: Int
  }
  deriving (Eq, Ord)

data Reserve = Reserve
  { _reserveValues   :: VMap Values
  , _reserveInactive :: ValueKind
  }

type MMap a = Map a Int
type VMap a = Map ValueKind a
type Values = [Value]
type Random = [Int]

makeLenses ''Unit
makeLenses ''PlayerName
makeLenses ''Reserve
makeLenses ''Value
makeLenses ''ValueKind

instance Show ValueKind where
  show = _valueKindName

instance Show PlayerName where
  show = _unPlayerName

instance Show Unit where
  show = _unitName

showRank :: Int -> ShowS
showRank i = (['A'..] !! i :)

instance Show Value where
  showsPrec _ v = showRank (v ^. valueRank) . (v ^. valueKind . valueKindName ++)

data PlayerStats = PlayerStats
  { _sellCounter :: Sum Int
  , _buyCounter  :: Sum Int
  }

instance Monoid PlayerStats where
  mempty = PlayerStats nil nil
  mappend (PlayerStats x0 y0) (PlayerStats x1 y1) = PlayerStats (x0 <> x1) (y0 <> y1)

data Player = Player
  { _playerName  :: PlayerName
  , _playerDate  :: Int
  , _playerVals  :: MMap Value
  , _playerCash  :: MMap Unit
  , _playerStats :: PlayerStats
  }

data Game = Game
  { _players :: Map PlayerName Player
  , _reserve :: Reserve
  , _random  :: Random
  , _stats   :: Int
  }

data Exch = Exch
  { _sellerName :: PlayerName
  , _sellValue  :: Value
  , _sellAmount :: Int
  , _buyPrice   :: Int
  , _buyUnit    :: Unit
  , _buyerName  :: PlayerName
  }

data Move
  = DoExch Exch
  -- Timeout |

data Focus = Focus
  { _focusValues  :: MMap Value
  , _focusReserve :: Reserve
  , _focusRandom  :: Random
  , _focusCombos  :: Int
  }

makeLenses ''Player
makeLenses ''PlayerStats
makeLenses ''Game
makeLenses ''Exch
makeLenses ''Move
makeLenses ''Focus
makePrisms ''Focus

instance Show Exch where
  show s = unwords [ s ^. sellerName . unPlayerName
                   , "sell"
                   , s ^. sellAmount . showed
                   , s ^. sellValue . showed
                   , "to"
                   , s ^. buyerName  . unPlayerName
                   , "at"
                   , s ^. buyPrice . showed
                   , s ^. buyUnit . unitName
                   ]

instance Show Move where
  showsPrec p (DoExch e) = showsPrec p e

instance Show Player where
  show p = unwords [ "player:"
                   , p ^. playerName . unPlayerName
                   , "age:"
                   , p ^. playerDate . showed
                   , "vals:"
                   , showMMap $ p ^. playerVals
                   , "cash:"
                   , showMMap $ p ^. playerCash
                   , "#sell:"
                   , p ^. playerStats . sellCounter . to getSum . showed
                   , "#buy:"
                   , p ^. playerStats . buyCounter . to getSum . showed
                   , "score:"
                   , p ^. playerScore allUnitsEqual . to getSum . showed
                   ]

instance Show Game where
  show (Game ps re ra st) = unlines . concat $
    [ "players:"  : ps ^.. each . showed
    , "reserve:"  : re ^.. reserveValues . each . showed
    , "inactive:" : [re ^. reserveInactive . showed]
    , "random:"   : [show (head ra) ++ "..."]
    , "stats:"    : [show st]
    ]

data LogItem
  = LogMove   Move
  | LogBoard  Game
  | LogString String

instance Show LogItem where
  showsPrec p = \case
    LogMove    m -> showsPrec p m
    LogBoard   g -> showsPrec p g
    LogString  s -> (s ++)

type Log = [LogItem]

class Say a where
  say :: MonadWriter Log m => a -> m ()

instance Say Move where say = tell . pure . LogMove
instance Say Game where say = tell . pure . LogBoard
instance a ~ Char => Say [a] where
  say = tell . pure . LogString
instance Say a => Say (Sum a) where
  say = say . getSum

{-
  These are ASSUMED
  * Existence of players
  * The selling player has the value to be sold
  * The buying player has the amount to buy it
-}
move :: MonadState Game m => Move -> m ()
move (DoExch e) = do
  players . at (e ^. sellerName) . _Just %= onSeller
  players . at (e ^. buyerName)  . _Just %= onBuyer
  where
    onSeller p = p & playerVals . at0 (e ^. sellValue) -~ (e ^. sellAmount)
                   & playerCash . at0 (e ^. buyUnit)   +~ (e ^. buyPrice)
                   & playerStats . sellCounter +~ 1
    onBuyer  p = p & playerVals . at0 (e ^. sellValue) +~ (e ^. sellAmount)
                   & playerCash . at0 (e ^. buyUnit)   -~ (e ^. buyPrice)
                   & playerStats . buyCounter +~ 1

-- ASSUMING that playerVals and playerCash only hold strictly positive
-- values, then all the generated moves should be legal.
nextMove :: (MonadState Game m, MonadLogic m) => m Move
nextMove = do
  allPlayers <- use $ players . each . to pure
  buyers <- permuteBuyers allPlayers
  buyer <- oneOf buyers
  let potentialSellers = filter (\seller -> seller ^. playerName /= buyer ^. playerName) allPlayers
  sellers <- permuteSellers buyer potentialSellers
  seller <- oneOf sellers
  values <- permuteSellerValues seller buyer
  value <- oneOf values
  (unit, balance) <- oneOf . sortOn snd $ buyer ^@.. playerCash . itraversed
  let price = valuePrice value
  guard $ balance >= price
  pure $ DoExch Exch
          { _sellerName = seller ^. playerName
          , _sellValue  = value
          , _sellAmount = 1
          , _buyPrice   = price
          , _buyUnit    = unit
          , _buyerName  = buyer ^. playerName
          }
  where
    oneOf = msum . map pure
 -- oneOf = foldr interleave mzero . map pure

    -- REMARK: this is not a perfect shuffle
    sortOnRnd :: (MonadState Game m, Ord b{-, Show c, Show d-}) =>
                 -- String -> Getting c a c -> Getting d b d ->
                 (a -> Int -> b) -> [a] -> m [a]
    sortOnRnd {-msg ga gb-} f xs = do
      ra <- use random
      random %= drop (length xs)
      let res = sortOn snd $ zipWith (\x r -> (x, f x r)) xs ra
      pure $ map fst res

    -- How much we know what to buy...
    buyerValue0 = valueCost

    sellerScore buyer seller rnd =
      ( Down $ maxMap (buyerValue1 buyer) (seller ^. playerVals)
      , rnd
      , Down $ seller ^. playerStats . sellCounter
      -- ^ Down: we prefer to sell to those who made more sales (converges faster)
      --   Removing Down would be more inclusive
      , Down $ seller ^. playerVals . sumMap (sellerValue seller)
      )
    -- How much do we value the stuff we do not have... (the seller has)
    buyerValue1 buyer v n
      | n' <- buyer ^. playerVals . at0 v
      , n' > 0 = (n' - n) * maxRank
      | otherwise = 0

    sellerValue _ v = valueCost v . (comboSize -)
    buyerScore buyer rnd =
      ( rnd `mod` numPlayers
      , Down $ buyer ^. playerCash . sumMap (const id)
      , Down $ buyer ^. playerVals . sumMap buyerValue0
      , Down $ buyer ^. playerStats . buyCounter
      -- ^ Down: we prefer to buy from those who made more sales (converges faster)
      --   Removing Down would be more inclusive
      )
    permuteSellers      = sortOnRnd
                            -- "sellers" playerName (to (\(Down x, y) -> (x, y)))
                            . sellerScore
    permuteBuyers       = sortOnRnd
                            -- "buyers"  playerName (to (\(Down (Sum x), Sum y, Down (Sum z)) -> (x, y, z)))
                            buyerScore
    permuteSellerValues seller buyer =
      fmap (map fst) . sortOnRnd {-"values" id id-} (compareSellerValue seller buyer)
                     $ seller ^@.. playerVals . itraversed
    -- TODO align the scale between ages and ranks
    compareSellerValue seller buyer (v, n) rnd =
      ( n
      , Down $ buyerValue1 buyer v n
      , Down $ diff (v ^. valueRank) (seller ^. playerDate)
      , diff (v ^. valueRank) (buyer ^. playerDate)
      , rnd
      )

mkPlayerMap :: [Player] -> Map PlayerName Player
mkPlayerMap = Map.fromList . map f
  where
    f p = (p ^. playerName, p)

dealValues :: MonadState Focus m => ValueKind -> Int -> m ()
dealValues kind amount = replicateM_ amount $ dealValue kind

dealValue :: MonadState Focus m => ValueKind -> m (Maybe Value)
dealValue kind = do
  maybev <- zoom' (mergeNonOverlappingOptics (focusReserve . reserveValues . atNil kind)
                                             focusRandom) pick
  forM_ maybev $ \v -> focusValues . at0 v += 1
  pure maybev

focusPlayerVals :: PlayerName -> Lens' Game Focus
focusPlayerVals name =
  merge4NonOverlappingOptics (players . at name . _Just . playerVals) reserve random stats
  . from _Focus

--type ZoomVals m n a = (Zoom m n Focus Game, Functor (Zoomed m a))

--zoomPlayerVals :: ZoomVals m n a => PlayerName -> m a -> n a
zoomPlayerVals :: MonadState Game m => PlayerName -> StateT Focus m a -> m a
zoomPlayerVals name = zoom' $ focusPlayerVals name

createCash :: MonadState Game m => PlayerName -> Unit -> Int -> m ()
createCash name unit amount =
  players . at name . _Just . playerCash . at0 unit += amount

valueKinds :: [ValueKind]
valueKinds = [ ValueKind [k] | k <- {-"DHCS"-} "♦♥♣♠" ]

valueKindsCount :: Int
valueKindsCount = length valueKinds

comboSize :: Int
comboSize = 4

maxRank :: Int
maxRank = 13

numPlayers :: Int
numPlayers = 10

initialValuesCount :: Int
initialValuesCount = 4

initialCash :: Int
initialCash = 1

defaultDeckOfKind :: ValueKind -> Values
defaultDeckOfKind k = [ Value k r | r <- [1..maxRank], _ <- [1..comboSize] ]

valueKindRank :: ValueKind -> Int
valueKindRank k =
  case findIndex (== k) valueKinds of
    Just i  -> i
    Nothing -> error "valueKindRank"

nextValueKind :: ValueKind -> ValueKind
nextValueKind k = valueKinds !! ((valueKindRank k + 1) `mod` valueKindsCount)

firstValueKind :: ValueKind
firstValueKind = head valueKinds

-- one before the last one
breakthroughValueKind :: ValueKind
breakthroughValueKind = reverse valueKinds !! 1

sumMap :: Num n => (k -> v -> n) -> Fold (Map k v) (Sum n)
sumMap f = folding (fmap (Sum . uncurry f) . Map.toList)

-- ASSUMING that for all 'x' of type 'n', 'x >= 0'.
maxMap :: (Num n, Ord n) => (k -> v -> n) -> Map k v -> n
maxMap f m
  | Map.null m = 0
  | otherwise  = maximum (uncurry f <$> Map.toList m)

playerScore :: n ~ Int => (Unit -> n) -> Fold Player (Sum n)
playerScore unitPrice =
  playerVals . sumMap valueCost
    <<>>
  playerCash . sumMap (\u n -> n * unitPrice u)

allUnitsEqual :: Num n => Unit -> n
allUnitsEqual _ = 1

valuePrice :: Value -> Int
valuePrice v = 2 ^ valueKindRank (v ^. valueKind)

-- A pair cost 4 times as much as a single card
-- Three of a kind cost 4 times as much as a pair ...
valueCost :: Value -> Int -> Int
valueCost v n
  | n  <=  0  = 0
  | otherwise = 2 ^ (2 * (n - 1) + valueKindRank (v ^. valueKind))

gameScore :: (Unit -> Int) -> Fold Game (Sum Int)
gameScore unitPrice = players . each . playerScore unitPrice

emptyGame :: Random -> Game
emptyGame ra = Game
  { _players = mkPlayerMap ps
  , _reserve = Reserve
    { _reserveValues =
        Map.fromList
          [ (k, vs)
          | k <- valueKinds
          , let vs = defaultDeckOfKind k
          ]
    , _reserveInactive = last valueKinds
    }
  , _random = ra
  , _stats = 0
  }
  where
    ps = [ Player { _playerName = name
                  , _playerDate = gen
                  , _playerVals = Map.empty
                  , _playerCash = Map.empty
                  , _playerStats = nil
                  }
         | gen <- [1..numPlayers]
         , let name = PlayerName $ '#' : (if gen < 10 then ('0' :) else id) (show gen)
         ]

forEachPlayers :: MonadState Game m => (PlayerName -> m a) -> m ()
forEachPlayers f = mapM_ f =<< uses players Map.keys

initialSteps :: MonadState Game m => Unit -> Int -> m ()
initialSteps unit amount = do
  forEachPlayers $ \name -> do
    zoomPlayerVals name $ dealValues firstValueKind initialValuesCount
    createCash name unit amount

resolveCombos :: (MonadWriter Log m, MonadState Game m) => m ()
resolveCombos = forEachPlayers $ \name -> zoomPlayerVals name $ resolveCombosPlayer 0
  where
    resolveCombosPlayer depth
      | depth >= 100 = do
          error "resolveCombos: too deep"
          -- . ("TOO DEEP:" ++) . show =<< use (mergeNonOverlappingOptics focusValues focusReserve)
          -- trace ("resolveCombosPlayer: depth=" ++ show depth) (pure ())
      | otherwise = do
        (combos, p) <- uses focusValues $ Map.partition (== comboSize)
        focusValues .= p
        forM_ (Map.keys combos) $ \combo -> do
          let kind = combo ^. valueKind
          leftover <- uses (focusReserve . reserveValues . atNil kind) length
          focusReserve . reserveValues . atNil kind <>= replicate comboSize combo
          -- FIXME: we need a better rule here!
          -- comboSize - 1 is too low at the begining of the game but comboSize is too high later on.
          dealValues kind $ if leftover < 4 then comboSize - 1 else comboSize
          let next = nextValueKind kind
          inactive <- use $ focusReserve . reserveInactive
          dealValues next 1
          focusCombos += 1
          say $ unwords ["new combo:", show combo]
          when (next == inactive) $ do
            say $ unwords ["new breakthrough:", show inactive]
            focusReserve . reserveInactive .= nextValueKind next
          resolveCombosPlayer (depth + 1)

resolveBreakthrough :: MonadState Game m => ValueKind -> m ()
resolveBreakthrough inactive = do
  players . each . playerVals . itraversed %@= \v n -> if v ^. valueKind == inactive then 0 else n
  reserve . reserveValues . at inactive ?= defaultDeckOfKind inactive

{- This ASSUMES that all the units are worth the same -}
runGame :: (MonadWriter Log m, MonadState Game m, MonadLogic m) => Int -> m ()
runGame 0   = say =<< get
runGame gas = do
  score <- use $ gameScore allUnitsEqual
  say $ unwords ["score:", show (getSum score)
                ,"gas:", show gas]
  mv <- nextMove
  say mv
  move mv
  inactive0 <- use $ reserve . reserveInactive
  resolveCombos
  inactive1 <- use $ reserve . reserveInactive
  when (inactive0 /= inactive1) $ resolveBreakthrough inactive1
  runGame (gas - 1)

testRandomSeed :: Random
testRandomSeed = cycle [32,10,10,43,49,30,23,40,36,26,12,2,24,40,45,15,50,28,17,16,0,37,28,26,44,2,36,44,16,29,30,8,18,2,14,2,24,20,34,18,6,37,0,12,23,10,26,25,25,26,31,22,0,20,36,49,36,28,17,39,5,31,6,21,7,49,40,19,48,9,26,16,27,17,12,51,43,2,41,8,4,50,45,8,30,19,10,42,2,31,48,44,16,41,41,22,33,19,27,40]
  -- cycle [191,186])) --,89,173,69,150,166,122,127,6,45,25,139,55,184,106,61,129,61,166,196,81,19,144,181,48,189,94,49,48,75,30,137,194,0,170,82,129,73,194,112,20,46,79,44,37,137,111,195,47,23,150,139,177,184,50,108,139,136,156,54,191,139,49,93,14,70,154,94,50,68,128,51,111,81,124,2,130,193,9,23,113,115,151,194,113,134,13,109,175,111,64,39,133,65,141,169,188,7,90]))

testGame :: Game
testGame = execState (initialSteps (Unit "$") initialCash) (emptyGame testRandomSeed)

testRunGame :: Int -> Int -> Log
testRunGame n m = observe . execWriterT $ evalStateT g testGame
  where
    g = do
      say =<< get
      runGame n
      s <- use stats
      guard $ s >= m
      -- if s >= m then pure () else trace "BACKTRACKING..." mzero
      say $ unwords ["combos:", show s]

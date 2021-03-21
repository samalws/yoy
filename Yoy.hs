import Data.Maybe
import Data.List
import Control.Monad
import qualified Data.Map as M

data Loc = Loc { x :: Int, y :: Int } deriving (Show, Eq, Ord)

loc = Loc

origin = loc 0 0

-- offset l1 by l2
-- less pretentious people might call this "adding"
offsetLoc :: Loc -> Loc -> Loc
offsetLoc l1 l2 = loc (x l1 + x l2) (y l1 + y l2)

neg :: Loc -> Loc
neg l = loc (-(x l)) (-(y l))

directions :: [Loc]
directions = [loc 0 1, loc 1 0, neg (loc 0 1), neg (loc 1 0)]

adjacentLocs :: Loc -> [Loc]
adjacentLocs l = map (offsetLoc l) directions

locDist :: Loc -> Loc -> Int
locDist l1 l2 = abs (x l1 - x l2) + abs (y l1 - y l2)

-- does not check if locAllowed l is true, assumes it is
_locFlood :: [Loc] -> (Loc -> Bool) -> Loc -> [Loc]
_locFlood alreadyChecked locAllowed l = fst $ foldr f ([l],l:alreadyChecked) toCheck where
  toCheck = filter locAllowed $ filter (not . flip elem alreadyChecked) $ adjacentLocs l
  f l2 (flooded,checked) = (union flooded results,union checked results) where
    results = _locFlood checked locAllowed l2

-- return a list of locs next to l, locs next those locs, etc, at which locAllowed is true
-- *does* check if locAllowed l is true
locFlood :: (Loc -> Bool) -> Loc -> [Loc]
locFlood locAllowed l = if (locAllowed l) then _locFlood [] locAllowed l else []

-- all locs within certain radius of center
locCircle :: Loc -> Int -> [Loc]
locCircle c r = locFlood (\l -> (locDist l c) <= r) c

type MapOf a = M.Map Loc a

data Player = Red | Blue deriving (Show, Eq)

data Unit = Unit { unitPlr :: Player, unitLvl :: Int } deriving (Show, Eq)
-- unit lvls start at 1

unitKills :: Unit -> Unit -> Bool
unitKills a b = (unitPlr a /= unitPlr b) && (unitLvl a > unitLvl b)

unitCostBuy :: Unit -> Int
unitCostBuy a = 4*(unitLvl a)

unitCostMaint :: Unit -> Int
unitCostMaint a = 2^(unitLvl a)

data GameBoard = GameBoard { locList :: [Loc], playerList :: [Player],
                             ownedLocs :: MapOf Player,
                             units :: MapOf Unit,
                             unitEnergies :: MapOf Int,
                             tileGroups :: MapOf Loc,
                             moneyInTileGroups :: MapOf Int
                             } deriving (Eq, Show)

-- if something happens on a given loc, update tileGroups for things near that loc
updateTileGroups :: Loc -> GameBoard -> GameBoard
updateTileGroups l g = g -- TODO

captureTile :: Loc -> Player -> GameBoard -> GameBoard
captureTile l p g = updateTileGroups l $ g { ownedLocs = M.insert l p (ownedLocs g) }

moveUnit :: Loc -> Loc -> GameBoard -> Maybe GameBoard
moveUnit l1 l2 g = do
  ug <- return $ units g

  -- unit in l1
  u <- M.lookup l1 ug

  -- unit in l2
  u2 <- return $ M.lookup l2 ug
  -- make sure u can kill u2, if u2 exists
  guard $ maybe True (unitKills u) u2

  -- check if we're staying in friendly territory, neutral territory, or enemy territory
  territoryPlr <- return $ M.lookup l2 $ ownedLocs g
  friendlyTerritory <- return $ maybe False (== unitPlr u) territoryPlr
  neutralTerritory <- return $ isNothing territoryPlr

  energies <- return $ unitEnergies g
  -- unit's energy left
  -- if none is left, then we automatically get Nothing, since we don't set 0s
  energy <- M.lookup l1 energies
  -- set new energy at l2, unset at l1
  -- when we're going into foreign territory, energy goes down to 0
  -- when energy goes down to 0, it's deleted
  newEnergies <- return $ M.delete l1 $
    (if (friendlyTerritory && energy > 1) then (M.insert l2 $ energy-1) else (M.delete l2)) energies

  -- check l2 not being guarded
  unitsNearby <- return $ catMaybes $ map (flip M.lookup ug) $ adjacentLocs l2
  unitsDefending <- return $ filter ((== territoryPlr) . Just . unitPlr) unitsNearby
  unbeatableUnitsDefending <- return $ filter (not . unitKills u) unitsDefending
  guard $ friendlyTerritory || length unbeatableUnitsDefending == 0

  -- the 2 spots are adjacent
  guard $ elem l2 $ adjacentLocs l1
  -- l2 is on the map
  guard $ elem l2 $ locList g

  newUnits <- return $ M.insert l2 u $ M.delete l1 ug
  return $ captureTile l2 (unitPlr u) $ g { units = newUnits, unitEnergies = newEnergies }

buyUnit :: Unit -> Loc -> GameBoard -> Maybe GameBoard
buyUnit u l g = Nothing -- TODO

currentMoney :: Loc -> GameBoard -> Maybe Int
currentMoney loc g = do
  addTo <- M.lookup loc $ tileGroups g
  return $ maybe 0 id $ M.lookup addTo $ moneyInTileGroups g

addMoney :: Int -> Loc -> GameBoard -> Maybe GameBoard
addMoney amt loc g = do
  money <- currentMoney loc g
  addTo <- M.lookup loc $ tileGroups g
  return $ g { moneyInTileGroups = M.insert addTo (money + amt) $ moneyInTileGroups g }

income :: Player -> GameBoard -> GameBoard
income p g = foldr f g spots where
  ownedPairings = M.toList $ ownedLocs g
  spots = map fst $ filter ((== p) . snd) ownedPairings
  f spot g2 = maybe g2 id $ addMoney 1 spot g2

feedUnits :: Player -> GameBoard -> GameBoard
feedUnits p g = g -- TODO

purgeUnits :: Player -> GameBoard -> GameBoard
purgeUnits p g = g -- TODO

updateMoney :: Player -> GameBoard -> GameBoard
updateMoney p = purgeUnits p . feedUnits p . income p

refreshUnitEnergies :: Player -> GameBoard -> GameBoard
refreshUnitEnergies p g = g { unitEnergies = newEnergies } where
  newEnergies = foldr f (unitEnergies g) spots
  unitPairings = M.toList $ units g
  spots = map fst $ filter ((== p) . unitPlr . snd) unitPairings
  f spot energies = M.insert spot 5 energies

switchTurn :: GameBoard -> GameBoard
switchTurn g = case (playerList g) of
  [] -> g
  (dran:rest) -> refreshUnitEnergies newDran $ updateMoney newDran $ g { playerList = newPlrs } where
    newPlrs = rest ++ [dran]
    newDran = head newPlrs

blankGame = GameBoard { locList = [], playerList = [],
                        ownedLocs = M.empty,
                        units = M.empty,
                        unitEnergies = M.empty,
                        tileGroups = M.empty,
                        moneyInTileGroups = M.empty
                        }

testGame = blankGame { locList = locCircle origin 2, playerList = [Red,Blue],
                       ownedLocs = M.fromList [(loc 1 1,Red),(loc (-1) (-1),Blue),(loc (-1) 0,Blue)],
                       units = M.fromList [(loc 1 1,Unit Red 1),(loc (-1) (-1),Unit Blue 1)],
                       unitEnergies = M.fromList [(loc (-1) (-1),2)] }



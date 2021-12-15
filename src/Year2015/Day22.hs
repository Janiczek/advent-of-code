{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Year2015Day22 (main) where

import Data.List (find)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as MinQueue


data Spell
  = MagicMissile
  | Drain
  | Shield
  | Poison
  | Recharge
  deriving (Show, Enum, Bounded, Eq)

newtype Spells = Spells
  { unSpells :: [Spell]
  }
  deriving (Show, Eq)

instance Semigroup Spells where
  (Spells x) <> (Spells y) = Spells (x ++ y)

instance Monoid Spells where
  mempty = Spells []

instance Ord Spells where
  compare = comparing overallCost

allSpells :: [Spell]
allSpells = [minBound .. maxBound]

data Effect
  = ShieldE
  | PoisonE
  | RechargeE
  deriving (Show, Eq)

manaCost :: Spell -> Int
manaCost spell =
  case spell of
    MagicMissile -> 53
    Drain -> 73
    Shield -> 113
    Poison -> 173
    Recharge -> 229

duration :: Effect -> Int
duration effect =
  case effect of
    ShieldE -> 6
    PoisonE -> 6
    RechargeE -> 5

applySpell :: Spell -> (Player, Boss) -> GameState (Player, Boss)
applySpell spell (player, boss) =
  case spell of
    MagicMissile ->
      StillGoing
        ( player,
          doDamage 4 boss
        )
    Drain ->
      StillGoing
        ( heal 2 player,
          doDamage 2 boss
        )
    Shield -> do
      player' <- addEffect ShieldE player
      return (player', boss)
    Poison -> do
      player' <- addEffect PoisonE player
      return (player', boss)
    Recharge -> do
      player' <- addEffect RechargeE player
      return (player', boss)

applyEffect :: Effect -> (Player, Boss) -> (Player, Boss)
applyEffect effect (player, boss) =
  case effect of
    ShieldE ->
      ( addArmor 7 player,
        boss
      )
    PoisonE ->
      ( player,
        doDamage 3 boss
      )
    RechargeE ->
      ( addMana 101 player,
        boss
      )

data Player = Player
  { playerHp :: Int,
    playerMana :: Int,
    playerArmor :: Int,
    playerEffects :: [(Effect, Int)]
  }
  deriving (Show, Eq)

instance Ord Player where
  compare = comparing playerHp

data Boss = Boss
  { bossHp :: Int,
    bossDmg :: Int
  }
  deriving (Show, Eq, Ord)

addEffect :: Effect -> Player -> GameState Player
addEffect effect player@Player {..} =
  case find (\(e, _) -> e == effect) playerEffects of
    Nothing ->
      StillGoing $
        player {playerEffects = (effect, duration effect) : playerEffects}
    Just _ -> BossWin

addMana :: Int -> Player -> Player
addMana mana player@Player {..} =
  player {playerMana = playerMana + mana}

addArmor :: Int -> Player -> Player
addArmor armor player@Player {..} =
  player {playerArmor = playerArmor + armor}

doDamage :: Int -> Boss -> Boss
doDamage dmg boss@Boss {..} =
  boss {bossHp = bossHp - dmg}

doDamageToPlayer :: Int -> Player -> Player
doDamageToPlayer dmg player@Player {..} =
  player {playerHp = playerHp - dmg}

heal :: Int -> Player -> Player
heal amount player@Player {..} =
  player {playerHp = playerHp + amount}

tryCastSpell :: Spell -> Player -> GameState Player
tryCastSpell spell player@Player {..} =
  let cost = manaCost spell
   in if playerMana >= cost
        then StillGoing (player {playerMana = playerMana - cost})
        else BossWin

resetArmor :: Player -> Player
resetArmor player =
  player {playerArmor = 0}

decrementEffects :: Player -> Player
decrementEffects player@Player {..} =
  let newEffects =
        mapMaybe
          ( \(e, t) ->
              if t > 1
                then Just (e, t -1)
                else Nothing
          )
          playerEffects
   in player {playerEffects = newEffects}

takePlayerTurn :: GameMode -> Spell -> (Player, Boss) -> GameState (Player, Boss)
takePlayerTurn gameMode spell (player, boss) = do
  let playerAfterDrain = 
        case gameMode of
          Easy -> player
          Hard -> doDamageToPlayer 1 player
  playerAfterPossibleDeath <- maybeKillPlayer playerAfterDrain
  let playerWithResetArmor = resetArmor playerAfterPossibleDeath
  let (playerAfterEffects, bossAfterEffects) =
        foldl
          (flip applyEffect)
          (playerWithResetArmor, boss)
          (map fst (playerEffects playerWithResetArmor))
  let playerWithDecEffects = decrementEffects playerAfterEffects
  bossAfterPossibleDeath1 <- maybeKillBoss bossAfterEffects
  playerWithLessMana <- tryCastSpell spell playerWithDecEffects
  (playerAfterSpell, bossAfterSpell) <- applySpell spell (playerWithLessMana, bossAfterPossibleDeath1)
  bossAfterPossibleDeath2 <- maybeKillBoss bossAfterSpell
  return (playerAfterSpell, bossAfterPossibleDeath2)

takeBossTurn :: (Player, Boss) -> GameState (Player, Boss)
takeBossTurn (player, boss@Boss {..}) = do
  let playerWithResetArmor = resetArmor player
  let (playerAfterEffects, bossAfterEffects) =
        foldl
          (flip applyEffect)
          (playerWithResetArmor, boss)
          (map fst (playerEffects playerWithResetArmor))
  let playerWithDecEffects = decrementEffects playerAfterEffects
  bossAfterPossibleDeath <- maybeKillBoss bossAfterEffects
  let dmgGiven = max 1 (bossDmg - playerArmor playerWithDecEffects)
  let playerAfterDmg = doDamageToPlayer dmgGiven playerWithDecEffects
  playerAfterPossibleDeath <- maybeKillPlayer playerAfterDmg
  return (playerAfterPossibleDeath, bossAfterPossibleDeath)

maybeKillPlayer :: Player -> GameState Player
maybeKillPlayer player@Player {..} =
  if playerHp <= 0
    then BossWin
    else StillGoing player

maybeKillBoss :: Boss -> GameState Boss
maybeKillBoss boss@Boss {..} =
  if bossHp <= 0
    then PlayerWin
    else StillGoing boss

data GameState a
  = PlayerWin
  | BossWin
  | StillGoing a
  deriving (Show, Ord, Eq)

instance Functor GameState where
  fmap _ PlayerWin = PlayerWin
  fmap _ BossWin = BossWin
  fmap f (StillGoing x) = StillGoing (f x)

instance Applicative GameState where
  pure = StillGoing
  PlayerWin <*> _ = PlayerWin
  BossWin <*> _ = BossWin
  StillGoing x <*> y = fmap x y

instance Monad GameState where
  PlayerWin >>= _ = PlayerWin
  BossWin >>= _ = BossWin
  StillGoing x >>= f = f x

step :: GameMode -> Spell -> (Player, Boss) -> GameState (Player, Boss)
step gameMode spell pb0 = do
  pb1 <- takePlayerTurn gameMode spell pb0
  takeBossTurn pb1

initPlayer :: Int -> Int -> Player
initPlayer hp mana =
  Player hp mana 0 []

overallCost :: Spells -> Int
overallCost (Spells spells) =
  sum $ map manaCost spells

stepMany :: GameMode -> [Spell] -> (Player, Boss) -> GameState (Player, Boss)
stepMany _ [] pb = StillGoing pb
stepMany gameMode (spell : rest) pb0 = do
  pb1 <- step gameMode spell pb0
  stepMany gameMode rest pb1

play :: GameMode -> (Player, Boss) -> [Spell] -> (GameState (Player, Boss), Int)
play gameMode pb spells =
  ( stepMany gameMode spells pb,
    overallCost (Spells spells)
  )

initPb :: (Player, Boss)
initPb =
  --(initPlayer 10 250, Boss 13 8)
  --(initPlayer 10 250, Boss 14 8)
  (initPlayer 50 500, Boss 58 9)



go :: GameMode -> MinQueue (GameState (Player, Boss), Spells) -> Maybe (Spells, Int) -> (Spells, Int)
go gameMode q maybeMinCost =
  case (MinQueue.minView q, maybeMinCost) of
    (Nothing, Nothing) -> error "queue ended, no wins"
    (Nothing, Just minCost) -> minCost
    (Just ((game, history), rest), _) ->
          case game of
            PlayerWin ->
              let cost = overallCost history
                  newMinCost =
                    case maybeMinCost of
                      Nothing -> Just (history, cost)
                      Just (minHistory, minCost) ->
                        Just $
                          if cost < minCost
                            then (history, cost)
                            else (minHistory, minCost)
               in go gameMode rest newMinCost
            BossWin -> error "boss win in `go` shouldn't happen"
            StillGoing pb ->
              let cost = overallCost history
               in case maybeMinCost of
                    Just (_, minCost)
                      | minCost < cost ->
                        go gameMode rest maybeMinCost
                    _ ->
                      let newHistories =
                            mapMaybe
                              ( \spell ->
                                   case step gameMode spell pb of
                                      BossWin -> Nothing
                                      other -> Just (other, history <> Spells [spell])
                              )
                              allSpells
                       in go
                            gameMode
                            ( MinQueue.union
                                rest
                                (MinQueue.fromList newHistories)
                            )
                            maybeMinCost

data GameMode
  = Easy
  | Hard

startPart1 :: (Player, Boss) -> (Spells, Int)
startPart1 pb =
  start Easy pb

startPart2 :: (Player, Boss) -> (Spells, Int)
startPart2 pb =
  start Hard pb

start :: GameMode -> (Player, Boss) -> (Spells, Int)
start gameMode pb =
  go
    gameMode
    ( MinQueue.fromList $ fmap (\spell -> (step gameMode spell pb, Spells [spell])) allSpells)
    Nothing

main :: IO ()
main =
  print $ startPart2 initPb

--print $
--  play
--    --(initPlayer 10 250, Boss 13 8)
--    --[Poison, MagicMissile]
--    (initPlayer 10 250, Boss 14 8)
--    [Recharge, Shield, Drain, Poison, MagicMissile]

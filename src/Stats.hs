{-# LANGUAGE RecordWildCards #-}

module Stats
  ( BotStats(..)
  , TournamentStats(..)
  , emptyBotStats
  , emptyTournamentStats
  , incrementShotsFired
  , incrementHitsLanded
  , addDamageDealt
  , addDamageTaken
  , addTimeAlive
  , setWinner
  , setDuration
  , renderTournamentStats
  , renderAggregateStats
  ) where

import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import           Data.List (foldl', intercalate)
import           Text.Printf (printf)

data BotStats = BotStats
  { shotsFired  :: !Int
  , hitsLanded  :: !Int
  , damageDealt :: !Int
  , damageTaken :: !Int
  , timeAlive   :: !Float
  } deriving (Eq, Show)

data TournamentStats = TournamentStats
  { perBot   :: !(Map Int BotStats)
  , winnerId :: !(Maybe Int)
  , duration :: !Float
  } deriving (Eq, Show)

emptyBotStats :: BotStats
emptyBotStats = BotStats 0 0 0 0 0

emptyTournamentStats :: TournamentStats
emptyTournamentStats = TournamentStats mempty Nothing 0

incrementShotsFired :: Int -> TournamentStats -> TournamentStats
incrementShotsFired = adjustBotStats $ \bs -> bs { shotsFired = shotsFired bs + 1 }

incrementHitsLanded :: Int -> TournamentStats -> TournamentStats
incrementHitsLanded = adjustBotStats $ \bs -> bs { hitsLanded = hitsLanded bs + 1 }

addDamageDealt :: Int -> Int -> TournamentStats -> TournamentStats
addDamageDealt botId amount =
  adjustBotStats (\bs -> bs { damageDealt = damageDealt bs + amount }) botId

addDamageTaken :: Int -> Int -> TournamentStats -> TournamentStats
addDamageTaken botId amount =
  adjustBotStats (\bs -> bs { damageTaken = damageTaken bs + amount }) botId

addTimeAlive :: Int -> Float -> TournamentStats -> TournamentStats
addTimeAlive botId dt =
  adjustBotStats (\bs -> bs { timeAlive = timeAlive bs + dt }) botId

setWinner :: Maybe Int -> TournamentStats -> TournamentStats
setWinner wId ts = ts { winnerId = wId }

setDuration :: Float -> TournamentStats -> TournamentStats
setDuration d ts = ts { duration = d }

adjustBotStats :: (BotStats -> BotStats) -> Int -> TournamentStats -> TournamentStats
adjustBotStats f botId ts@TournamentStats{..} =
  let perBot' = M.alter (Just . f . maybe emptyBotStats id) botId perBot
  in ts { perBot = perBot' }

renderTournamentStats :: Int -> TournamentStats -> String
renderTournamentStats index TournamentStats{..} =
  let header = printf "===== Torneo %d =====\n" index
      durationLine = printf "Duracion: %.2f s\n" duration
      winnerLine = "Ganador: " ++ maybe "None" (\bid -> "Bot " ++ show bid) winnerId ++ "\n"
      botLines = map (uncurry formatBot) (M.toAscList perBot)
  in header ++ durationLine ++ winnerLine ++ concat botLines ++ "\n"
  where
    formatBot :: Int -> BotStats -> String
    formatBot botId BotStats{..} =
      let lifePct = if duration <= 0 then 0 else (timeAlive / duration) * 100
      in printf "[Bot %d] shots=%d, hits=%d, dmgDealt=%d, dmgTaken=%d, timeAlive=%.2fs, lifePct=%.1f%%\n"
                 botId shotsFired hitsLanded damageDealt damageTaken timeAlive lifePct

renderAggregateStats :: [TournamentStats] -> String
renderAggregateStats tournaments =
  let totalTournaments = length tournaments
      header = printf "===== Resumen (%d torneos) =====\n" totalTournaments
      botAggregateMap = buildBotAggregate tournaments
      botLines = map (uncurry formatAggregate) (M.toAscList botAggregateMap)
      durations = map duration tournaments
      avgDuration = if null durations then 0 else sum durations / fromIntegral totalTournaments
      maxDuration = if null durations then 0 else maximum durations
      winsMap = buildWinsMap tournaments
      winsStr = "{" ++ intercalate ", " (map formatWin (M.toAscList winsMap)) ++ "}"
      globalLine = printf "Global: avgDuration=%.2f s, maxDuration=%.2f s, winsPorBot=%s\n\n"
                          avgDuration maxDuration winsStr
  in header ++ concat botLines ++ globalLine
  where
    formatWin :: (Int, Int) -> String
    formatWin (botId, wins) = show botId ++ ":" ++ show wins

data BotAggregate = BotAggregate
  { aggCount             :: !Int
  , aggShotsTotal        :: !Int
  , aggHitsTotal         :: !Int
  , aggDamageDealtTotal  :: !Int
  , aggDamageTakenTotal  :: !Int
  , aggTimeAliveTotal    :: !Float
  , aggLifePctTotal      :: !Float
  , aggShotsMax          :: !Int
  , aggHitsMax           :: !Int
  , aggDamageDealtMax    :: !Int
  , aggDamageTakenMax    :: !Int
  , aggTimeAliveMax      :: !Float
  , aggLifePctMax        :: !Float
  }
buildBotAggregate :: [TournamentStats] -> Map Int BotAggregate
buildBotAggregate = foldl' accumulateTournament mempty
  where
    accumulateTournament :: Map Int BotAggregate -> TournamentStats -> Map Int BotAggregate
    accumulateTournament acc TournamentStats{..} =
      foldl' (accumulateBot duration) acc (M.toAscList perBot)

    accumulateBot :: Float -> Map Int BotAggregate -> (Int, BotStats) -> Map Int BotAggregate
    accumulateBot dur acc (botId, BotStats{..}) =
      let lifePct = if dur <= 0 then 0 else (timeAlive / dur) * 100
          updated = case M.lookup botId acc of
                      Nothing -> newAggregate lifePct
                      Just agg -> mergeAggregate agg lifePct
      in M.insert botId updated acc
      where
        newAggregate lifePct =
          BotAggregate
            { aggCount            = 1
            , aggShotsTotal       = shotsFired
            , aggHitsTotal        = hitsLanded
            , aggDamageDealtTotal = damageDealt
            , aggDamageTakenTotal = damageTaken
            , aggTimeAliveTotal   = timeAlive
            , aggLifePctTotal     = lifePct
            , aggShotsMax         = shotsFired
            , aggHitsMax          = hitsLanded
            , aggDamageDealtMax   = damageDealt
            , aggDamageTakenMax   = damageTaken
            , aggTimeAliveMax     = timeAlive
            , aggLifePctMax       = lifePct
            }

        mergeAggregate agg lifePct = agg
          { aggCount            = aggCount agg + 1
          , aggShotsTotal       = aggShotsTotal agg + shotsFired
          , aggHitsTotal        = aggHitsTotal agg + hitsLanded
          , aggDamageDealtTotal = aggDamageDealtTotal agg + damageDealt
          , aggDamageTakenTotal = aggDamageTakenTotal agg + damageTaken
          , aggTimeAliveTotal   = aggTimeAliveTotal agg + timeAlive
          , aggLifePctTotal     = aggLifePctTotal agg + lifePct
          , aggShotsMax         = max (aggShotsMax agg) shotsFired
          , aggHitsMax          = max (aggHitsMax agg) hitsLanded
          , aggDamageDealtMax   = max (aggDamageDealtMax agg) damageDealt
          , aggDamageTakenMax   = max (aggDamageTakenMax agg) damageTaken
          , aggTimeAliveMax     = max (aggTimeAliveMax agg) timeAlive
          , aggLifePctMax       = max (aggLifePctMax agg) lifePct
          }

buildWinsMap :: [TournamentStats] -> Map Int Int
buildWinsMap = foldl' registerWinner mempty
  where
    registerWinner acc TournamentStats{ winnerId = Nothing } = acc
    registerWinner acc TournamentStats{ winnerId = Just botId } =
      M.insertWith (+) botId 1 acc

formatAggregate :: Int -> BotAggregate -> String
formatAggregate botId BotAggregate{..}
  | aggCount == 0 = ""
  | otherwise =
      let countF = fromIntegral aggCount :: Float
          avgShots = fromIntegral aggShotsTotal / countF
          avgHits  = fromIntegral aggHitsTotal / countF
          avgDmgDealt = fromIntegral aggDamageDealtTotal / countF
          avgDmgTaken = fromIntegral aggDamageTakenTotal / countF
          avgTimeAlive = aggTimeAliveTotal / countF
          avgLifePct = aggLifePctTotal / countF
      in printf "[Bot %d] avg(shots)=%.2f, max(shots)=%d, avg(hits)=%.2f, max(hits)=%d, avg(dmgDealt)=%.2f, max(dmgDealt)=%d, avg(dmgTaken)=%.2f, max(dmgTaken)=%d, avg(timeAlive)=%.2fs, max(timeAlive)=%.2fs, avg(lifePct)=%.1f%%, max(lifePct)=%.1f%%, total(dmgDealt)=%d, total(dmgTaken)=%d\n"
                 botId avgShots aggShotsMax avgHits aggHitsMax avgDmgDealt aggDamageDealtMax avgDmgTaken aggDamageTakenMax avgTimeAlive aggTimeAliveMax avgLifePct aggLifePctMax aggDamageDealtTotal aggDamageTakenTotal



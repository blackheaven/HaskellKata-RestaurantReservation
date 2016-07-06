{-# LANGUAGE DeriveFunctor #-}

module CaravanService
  (
    CaravanServiceAction
  , ServiceAddress
  , findCaravan
  , reserveCaravan
  , runCaravanServiceFile
  ) where

import Control.Monad.Free (liftF, Free, iterM)
import Data.Time (ZonedTime)
import Data.List (find)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Utils
import Types

data CaravanServiceActionF next =
    Find Int ZonedTime        (Maybe Caravan -> next)
  | Reserve ZonedTime Caravan next
  deriving (Functor)

type CaravanServiceAction = Free CaravanServiceActionF
type ServiceAddress = String


findCaravan :: Int -> ZonedTime -> CaravanServiceAction (Maybe Caravan)
findCaravan c t = liftF $ Find c t id

reserveCaravan :: ZonedTime -> Caravan -> CaravanServiceAction ()
reserveCaravan t c = liftF $ Reserve t c ()

runCaravanServiceFile :: ServiceAddress -> CaravanServiceAction a -> IO a
runCaravanServiceFile root = iterM $ \action ->
  case action of
    Find c t    n -> onFindCaravan root c t    >>= n
    Reserve t c n -> onReserveCaravan root t c >>  n


caravanPool :: [Caravan]
caravanPool = map Caravan [4, 6, 8]

fileNameForCaravan :: ZonedTime -> FilePath
fileNameForCaravan = (++ ".caravan.txt") . rawFileNameForDate

readReservedCaravans :: ServiceAddress -> ZonedTime -> IO [Caravan]
readReservedCaravans dir d = do -- Imagine that this queries a web service instead of reading from a file
  exists <- doesFileExist fileName
  if exists
    then read <$> readFile fileName
    else return []
  where fileName = dir </> fileNameForCaravan d

onFindCaravan :: ServiceAddress -> Int -> ZonedTime -> IO (Maybe Caravan)
onFindCaravan dir requestedCapacity d = do
  putStrLn "Finding a caravan..."
  reservedCaravans <- readReservedCaravans dir d
  let availableCaravans = filter (`notElem` reservedCaravans) caravanPool
  return $ find (\c -> requestedCapacity <= caravanCapacity c) availableCaravans

onReserveCaravan :: ServiceAddress -> ZonedTime -> Caravan -> IO ()
onReserveCaravan dir d c = do --Imagine that this updates a web service instead of writing to a file
    caravans <- readReservedCaravans dir d
    -- Use of `seq` as described in http://stackoverflow.com/a/2530948/126014
    length caravans `seq` writeFile fileName $ show (c : caravans)
  where fileName = dir </> fileNameForCaravan d

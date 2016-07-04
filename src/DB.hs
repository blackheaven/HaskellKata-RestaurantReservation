module DB
  (
    ConnectionString
  , readReservationsFromDB
  , getReservedSeatsFromDB
  , saveReservation
  , ServiceAddress
  , findCaravan
  , reserveCaravan
  ) where

import Data.List (find)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Data.Time (FormatTime(..), ZonedTime(..), formatTime, defaultTimeLocale)
import ApiModel

type ConnectionString = String

rawFileNameForDate :: FormatTime t => t -> String
rawFileNameForDate = formatTime defaultTimeLocale "%F"

fileNameForReservation :: Reservation -> FilePath
fileNameForReservation = (++ ".txt") . rawFileNameForDate . date

readReservationsFromDB :: ConnectionString -> ZonedTime -> IO [Reservation]
readReservationsFromDB dir d = do -- Imagine that this queries a database table instead of reading from a file
  exists <- doesFileExist fileName
  if exists
    then read <$> readFile fileName
    else return []
  where fileName = dir </> rawFileNameForDate d ++ ".txt"

getReservedSeatsFromDB :: ConnectionString -> ZonedTime -> IO Int
getReservedSeatsFromDB dir d = do
  reservations <- readReservationsFromDB dir d
  return (foldr ((+) . quantity) 0 reservations)

saveReservation :: ConnectionString -> Reservation -> IO ()
saveReservation dir r = do --Imagine that this inserts into a database table instead of writing to a file
    reservations <- readReservationsFromDB dir (date r)
    -- Use of `seq` as described in http://stackoverflow.com/a/2530948/126014
    length reservations `seq` writeFile fileName $ show (r : reservations)
  where fileName = dir </> fileNameForReservation r

-- Caravan storage

caravanPool :: [Caravan]
caravanPool = map Caravan [4, 6, 8]

fileNameForCaravan :: ZonedTime -> FilePath
fileNameForCaravan = (++ ".caravan.txt") . rawFileNameForDate

type ServiceAddress = String

readReservedCaravans :: ServiceAddress -> ZonedTime -> IO [Caravan]
readReservedCaravans dir d = do -- Imagine that this queries a web service instead of reading from a file
  exists <- doesFileExist fileName
  if exists
    then read <$> readFile fileName
    else return []
  where fileName = dir </> fileNameForCaravan d

findCaravan :: ServiceAddress -> Int -> ZonedTime -> IO (Maybe Caravan)
findCaravan dir requestedCapacity d = do
  putStrLn "Finding a caravan..."
  reservedCaravans <- readReservedCaravans dir d
  let availableCaravans = filter (`notElem` reservedCaravans) caravanPool
  return $ find (\c -> requestedCapacity <= caravanCapacity c) availableCaravans

reserveCaravan :: ServiceAddress -> ZonedTime -> Caravan -> IO ()
reserveCaravan dir d c = do --Imagine that this updates a web service instead of writing to a file
    caravans <- readReservedCaravans dir d
    -- Use of `seq` as described in http://stackoverflow.com/a/2530948/126014
    length caravans `seq` writeFile fileName $ show (c : caravans)
  where fileName = dir </> fileNameForCaravan d
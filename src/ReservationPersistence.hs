{-# LANGUAGE DeriveFunctor #-}

module ReservationPersistence
  (
    ReservationPersistenceAction
  , ConnectionString(..)
  , saveReservation
  , getReservedSeats
  , tryExtendCapacity
  , runReservationPersistenceFile
  ) where

import Control.Monad.Free (liftF, Free, iterM)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Data.Time (ZonedTime)
import Utils
import Types

data ReservationPersistenceActionF next =
    Save Reservation              next
  | FetchReservedSeats ZonedTime  (Int -> next)
  | TryExtendCapacity Reservation (Either Error Reservation -> next)
  deriving (Functor)

type ReservationPersistenceAction = Free ReservationPersistenceActionF
type ConnectionString = String

saveReservation :: Reservation -> ReservationPersistenceAction ()
saveReservation r = liftF $ Save r ()

getReservedSeats :: ZonedTime -> ReservationPersistenceAction Int
getReservedSeats t = liftF $ FetchReservedSeats t id

tryExtendCapacity :: Reservation -> ReservationPersistenceAction (Either Error Reservation)
tryExtendCapacity r = liftF $ TryExtendCapacity r id


runReservationPersistenceFile :: ConnectionString -> (Reservation -> IO (Either Error Reservation)) -> ReservationPersistenceAction a -> IO a
runReservationPersistenceFile root extension = iterM $ \action -> case action of
    FetchReservedSeats t n -> onGetReservedSeats root t >>= n
    Save r               n -> onSaveReservation root r  >>  n
    TryExtendCapacity r  n -> extension r               >>= n


fileNameForReservation :: Reservation -> FilePath
fileNameForReservation = (++ ".txt") . rawFileNameForDate . date

readReservationsFromDB :: ConnectionString -> ZonedTime -> IO [Reservation]
readReservationsFromDB dir d = do -- Imagine that this queries a database table instead of reading from a file
  exists <- doesFileExist fileName
  if exists
    then read <$> readFile fileName
    else return []
  where fileName = dir </> rawFileNameForDate d ++ ".txt"

onGetReservedSeats :: ConnectionString -> ZonedTime -> IO Int
onGetReservedSeats dir d = do
  reservations <- readReservationsFromDB dir d
  return (foldr ((+) . quantity) 0 reservations)

onSaveReservation :: ConnectionString -> Reservation -> IO ()
onSaveReservation dir r = do --Imagine that this inserts into a database table instead of writing to a file
    reservations <- readReservationsFromDB dir (date r)
    -- Use of `seq` as described in http://stackoverflow.com/a/2530948/126014
    length reservations `seq` writeFile fileName $ show (r : reservations)
  where fileName = dir </> fileNameForReservation r

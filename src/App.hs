module App
    (
      postReservation
    ) where

import ApiModel
import ReservationPersistence
import CaravanService
import Utils
import Types
import Control.Monad((>=>), liftM)
import Control.Monad.Trans.Either(EitherT(..), runEitherT, left, right)

connStr :: ConnectionString
connStr = "."

svcAddr :: ServiceAddress
svcAddr = "."

tryRentCaravan :: Reservation -> EitherT Error CaravanServiceAction Reservation
tryRentCaravan reservation = do
  foundCaravan <- EitherT $ fmap (maybeToEither CapacityExceeded id) $ findCaravan (quantity reservation) (date reservation)
  let newRes = checkCaravanCapacity reservation foundCaravan
  either left (\r -> EitherT (fmap return (reserveCaravan (date r) foundCaravan)) >> right r) newRes

postReservation :: ReservationRendition -> IO (HttpResult ())
postReservation =
  fmap toHttpResult . run . either (return . Left) (runEitherT . reserve) . validateReservation
  where run = runReservationPersistenceFile connStr (runCaravanServiceFile svcAddr . runEitherT . tryRentCaravan)

reserve :: Reservation -> EitherT Error ReservationPersistenceAction ()
reserve r = do
      i <- EitherT $ fmap return $ getReservedSeats $ date r
      newRes <- either (const $ EitherT $ tryExtendCapacity r) right (checkCapacity 10 i r)
      EitherT $ fmap return $ saveReservation newRes

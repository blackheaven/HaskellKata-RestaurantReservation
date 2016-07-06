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

connStr :: ConnectionString
connStr = "."

svcAddr :: ServiceAddress
svcAddr = "."

tryRentCaravan :: Reservation -> CaravanServiceAction (Either Error Reservation)
tryRentCaravan reservation = do
  foundCaravan <- findCaravan (quantity reservation) (date reservation)
  let eitherCaravan = maybeToEither CapacityExceeded id foundCaravan
  let newRes = eitherCaravan >>= \c -> (fmap (\r -> (c, r)) $ checkCaravanCapacity reservation c)
  either (return . Left) (\(c, r) -> (reserveCaravan (date r) c >> return (return r))) newRes
postReservation :: ReservationRendition -> IO (HttpResult ())
postReservation =
  fmap toHttpResult . run . either (return . Left) reserve . validateReservation
  where run = runReservationPersistenceFile connStr (runCaravanServiceFile svcAddr . tryRentCaravan)

reserve :: Reservation -> ReservationPersistenceAction (Either Error ())
reserve r = do
      i <- getReservedSeats $ date r
      newRes <- either (const $ tryExtendCapacity r) (return . return) (checkCapacity 10 i r)
      let rr = fmap saveReservation newRes
      return $ return ()

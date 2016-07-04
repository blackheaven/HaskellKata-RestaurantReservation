module App
    (
      postReservation
    ) where

import ApiModel
import DB
import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Either (EitherT(..), hoistEither, right, eitherT)

connStr :: ConnectionString
connStr = "."

svcAddr :: ServiceAddress
svcAddr = "."

checkCaravan :: Reservation -> Error -> EitherT Error IO Reservation
checkCaravan reservation err = do
  c <- liftIO $ findCaravan svcAddr (quantity reservation) (date reservation)
  newRes <- hoistEither $ checkCaravanCapacityOnError err c reservation
  liftIO $ forM_ c $ reserveCaravan svcAddr (date newRes)
  return newRes

postReservation :: ReservationRendition -> IO (HttpResult ())
postReservation candidate = fmap toHttpResult $ runEitherT $ do
  r <- hoistEither $ validateReservation candidate
  i <- liftIO $ getReservedSeatsFromDB connStr $ date r
  eitherT (checkCaravan r) right $ hoistEither $ checkCapacity 10 i r
  >>= liftIO . saveReservation connStr
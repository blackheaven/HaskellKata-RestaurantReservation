{-# LANGUAGE DeriveFunctor #-}
module ApiModel where

import Data.Time (ZonedTime(..), parseTimeM, defaultTimeLocale, iso8601DateFormat)

data ReservationContainer a = ReservationContainer
  { date :: a
  , name :: String
  , email :: String
  , quantity :: Int }
  deriving (Show, Read, Functor)

type ReservationRendition = ReservationContainer String
type Reservation = ReservationContainer ZonedTime

data Caravan = Caravan
  { caravanCapacity :: Int } -- Imagine that this type has more composite elements
  deriving (Eq, Show, Read)

data Error = ValidationError String | CapacityExceeded
             deriving (Show, Eq)

parseDate :: String -> Maybe ZonedTime
parseDate = parseTimeM True defaultTimeLocale (iso8601DateFormat Nothing)

validateReservation :: ReservationRendition -> Either Error Reservation
validateReservation r =
  case parseDate (date r) of
    Just d -> Right $ fmap (const d) r
    Nothing -> Left (ValidationError "Invalid date.")

checkCapacity :: Int -> Int -> Reservation -> Either Error Reservation
checkCapacity capacity reservedSeats reservation =
    if capacity < quantity reservation + reservedSeats
    then Left CapacityExceeded
    else Right reservation

checkCaravanCapacityOnError :: Error
                            -> Maybe Caravan
                            -> Reservation
                            -> Either Error Reservation
checkCaravanCapacityOnError CapacityExceeded (Just caravan) reservation =
  if caravanCapacity caravan < quantity reservation
  then Left CapacityExceeded
  else Right reservation
checkCaravanCapacityOnError err _ _ = Left err

data StatusCode = Forbidden | Accepted -- add more at leisure
                  deriving (Eq, Show, Read)

data HttpResult a = OK a
                  | BadRequest String
                  | StatusCode StatusCode
                  deriving (Eq, Show, Read)

toHttpResult :: Either Error () -> HttpResult ()
toHttpResult (Left (ValidationError msg)) = BadRequest msg
toHttpResult (Left CapacityExceeded) = StatusCode Forbidden
toHttpResult (Right ()) = OK ()

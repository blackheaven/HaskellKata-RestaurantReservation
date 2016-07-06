module ApiModel where

import Data.Time (ZonedTime(..), parseTimeM, defaultTimeLocale, iso8601DateFormat)
import Utils
import Types

parseDate :: String -> Maybe ZonedTime
parseDate = parseTimeM True defaultTimeLocale (iso8601DateFormat Nothing)

validateReservation :: ReservationRendition -> Either Error Reservation
validateReservation r =
  maybeToEither (ValidationError "Invalid date.") (\d -> fmap (const d) r) (parseDate (date r))

checkCapacity :: Int -> Int -> Reservation -> Either Error Reservation
checkCapacity capacity reservedSeats reservation =
    if capacity < quantity reservation + reservedSeats
    then Left CapacityExceeded
    else Right reservation

checkCaravanCapacity :: Reservation -> Caravan -> Either Error Reservation
checkCaravanCapacity reservation caravan =
  if caravanCapacity caravan < quantity reservation
  then Left CapacityExceeded
  else Right reservation

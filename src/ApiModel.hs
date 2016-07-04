module ApiModel where

import Data.Time (ZonedTime(..), parseTimeM, defaultTimeLocale, iso8601DateFormat)

data ReservationRendition = ReservationRendition
  { rDate :: String
  , rName :: String
  , rEmail :: String
  , rQuantity :: Int }
  deriving (Eq, Show, Read)

data Reservation = Reservation
  { date :: ZonedTime
  , name :: String
  , email :: String
  , quantity :: Int }
  deriving (Show, Read)

data Caravan = Caravan
  { caravanCapacity :: Int } -- Imagine that this type has more composite elements
  deriving (Eq, Show, Read)

instance Eq Reservation where
  x == y =
    zonedTimeZone (date x) == zonedTimeZone (date y) &&
    zonedTimeToLocalTime (date x) == zonedTimeToLocalTime (date y) &&
    name x == name y &&
    email x == email y &&
    quantity x == quantity y

data Error = ValidationError String | CapacityExceeded
             deriving (Show, Eq)

parseDate :: String -> Maybe ZonedTime
parseDate = parseTimeM True defaultTimeLocale (iso8601DateFormat Nothing)

validateReservation :: ReservationRendition -> Either Error Reservation
validateReservation r =
  case parseDate (rDate r) of
    Just d ->
      Right Reservation
        { date = d
        , name = rName r
        , email = rEmail r
        , quantity = rQuantity r }
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
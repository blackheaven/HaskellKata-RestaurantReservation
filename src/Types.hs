{-# LANGUAGE DeriveFunctor #-}
module Types where

import Data.Time (ZonedTime)

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

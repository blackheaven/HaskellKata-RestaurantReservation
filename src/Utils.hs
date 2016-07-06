module Utils where
import Data.Time (FormatTime(..), ZonedTime(..), formatTime, defaultTimeLocale)

maybeToEither :: e -> (a -> b) -> Maybe a -> Either e b
maybeToEither e f i = case i of
                         Just x  -> Right $ f x
                         Nothing -> Left e

rawFileNameForDate :: FormatTime t => t -> String
rawFileNameForDate = formatTime defaultTimeLocale "%F"

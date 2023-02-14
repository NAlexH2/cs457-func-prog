Name: Alex Harris
Date: 2/11/2023
Assignment 5

The goal of this short exercise is to get a bit more practice with using the
`Maybe` Monad.

> module MaybePractice where
> import Test.HUnit

> import qualified Control.Monad as Monad

> import Data.Map (Map)
> import qualified Data.Map as Map
> import qualified Text.Read as Text

Part 1
------

Consider the following `Weather` datatype:

> data Weather = Weather {
>     day :: Int, maxTemp :: Int, minTemp :: Int
>     } deriving (Eq, Show)

> example :: Weather
> example = Weather {day = 2, maxTemp = 78, minTemp = 62}

Use `Map.lookup` and `Text.readMaybe`
(https://hackage.haskell.org/package/base-4.17.0.0/docs/Text-Read.html#v:readMaybe)
to write a function that takes a dictionary (i.e. a finite map from `String`
keys to `String` values) and converts it into weather data. Your implementation
should not use pattern matching; use `do`-notation instead.

If the provided dictionary does not have the appropriate data, then the
function should return `Nothing`. In other words, this function should behave
according to the examples below.

-- example :: Weather
-- example = Weather {day = 2, maxTemp = 78, minTemp = 62}

> testParseWeather :: Test
> testParseWeather = "testParseWeather" ~:
>   TestList 
>   [ 
>     parseWeather 
>     (Map.fromList [("day", "2"), ("maxTemp", "78"), ("minTemp", "62")]) ~?= 
>     Just example,
>
>     parseWeather (Map.fromList [("day", "2")]) ~?= Nothing,
>
>     parseWeather 
>     (Map.fromList [("day", "two"), ("maxTemp", "78"), ("minTemp", "62")]) ~?= 
>     Nothing
>   ]

> parseWeather :: Map String String -> Maybe Weather
> parseWeather wMap = do
>       theDay <- Map.lookup "day" wMap
>       hTemp <- Map.lookup "maxTemp" wMap
>       lTemp <- Map.lookup "minTemp" wMap
>       tDM <- Text.readMaybe theDay
>       hTM <- Text.readMaybe hTemp
>       lTM <- Text.readMaybe lTemp
>       return (Weather tDM hTM lTM)


Part 2
------

Consider the following operations that combine two `Maybe`s. Implement them
both. One can be defined using `(>>=)` from the `Maybe` monad, and one cannot.
Which is which?


> -- | Left-biased choice on maybes
> --
> -- >>> firstJust (Just 1) (Just 2)
> -- Just 1
> -- >>> firstJust Nothing (Just 2)
> -- Just 2
> -- >>> firstJust (Just 1) Nothing
> -- Just 1
> -- >>> firstJust Nothing Nothing
> -- Nothing
> firstJust :: Maybe a -> Maybe a -> Maybe a
> firstJust = undefined


> -- | Ensure that both Maybes are 'Just' and retain the first one
> --
> -- >>> sequenceFirst (Just 1) (Just 'a')
> -- Just 1
> -- >>> sequenceFirst Nothing (Just 'a')
> -- Nothing
> -- >>> sequenceFirst (Just 1) Nothing
> -- Nothing
> -- >>> sequenceFirst Nothing Nothing
> -- Nothing

> sequenceFirst :: Maybe a -> Maybe b -> Maybe a
> sequenceFirst = undefined

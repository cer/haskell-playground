{-# LANGUAGE TypeFamilies #-}

module Aggregate ( Aggregate, Error, Event, Command, execute, apply, seed, processCommand, (<==)) where

-- Original code from https://gist.github.com/Fristi/7327904

import Data.Function (on)
import Control.Applicative
 
data EventData e = EventData {
    eventId :: Int,
    body :: Event e
}
 
instance Show (EventData e) where
    show = show . eventId
 
instance Eq (EventData e) where
    (==) = (==) `on` eventId
 
instance Ord (EventData e) where
    compare = compare `on` eventId
 
class Aggregate s where
    data Error s :: *
    data Command s :: *
    data Event s :: *
 
    execute :: s -> Command s -> Either (Error s) (Event s)
    apply :: s -> Event s -> s
    seed :: s


load :: (Aggregate a) => [EventData a] -> a
load = foldl folder seed
    where
        folder state = apply state . body

processCommand :: Aggregate s => s -> Command s -> s
processCommand s c = apply s (case execute s c of Right x -> x)

(<==) :: Aggregate s => s -> Command s -> s
(<==) s c = processCommand s c



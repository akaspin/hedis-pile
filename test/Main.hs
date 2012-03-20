
module Main (main) where

import Test.Framework (defaultMain)

import qualified Database.Redis.Test.Pile
import qualified Database.Redis.Test.PileBinary

main :: IO ()
main = defaultMain [
    Database.Redis.Test.Pile.tests,
    Database.Redis.Test.PileBinary.tests
    ]


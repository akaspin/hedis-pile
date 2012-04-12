
module Main (main) where

import Test.Framework (defaultMain)

import qualified Database.Redis.Test.Pile

main :: IO ()
main = defaultMain [
    Database.Redis.Test.Pile.tests
    ]


module Main where

import Test.Hspec
import System.Process
import qualified Spec

main :: IO ()
main = do
    _ <- system "cd ../../example/todo && make"
    hspec Spec.spec

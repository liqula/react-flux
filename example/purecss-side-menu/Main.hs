module Main (main) where

import React.Flux
import App
import NavStore (initHistory)

main :: IO ()
main = do
    initHistory
    reactRender "side-menu-app" myApp ()

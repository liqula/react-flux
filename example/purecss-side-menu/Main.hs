module Main (main) where

import React.Flux
import App

main :: IO ()
main = reactRender "side-menu-app" myApp ()

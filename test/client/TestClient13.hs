module Main (main) where

import TestClient
import React.Flux

main :: IO ()
main = do
    initializeTouchEvents
    reactRender "app" testClient ()

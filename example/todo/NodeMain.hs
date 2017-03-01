import React.Flux
import TodoViews
import qualified Data.Text.IO as T

main :: IO ()
main = reactRenderViewToString True todoApp >>= T.putStrLn

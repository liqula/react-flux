module React.Flux.DOM where


div_ :: [Pair] -> ReactElementM eventHandler a -> ReactElementM eventHandler a
div_ attrs = el "div" attrs []

input_ :: String -> [Pair] -> [EventHandler eventHandler] -> ReactElementM eventHandler
input_ ty attrs handlers = el "input" ("type" .= ty : attrs) handlers mzero


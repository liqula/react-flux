# 0.9.4

* Fix to build with latest ghcjs-base (requires at least aaa4d59117f37d1b9c60a154a9128b3bcc6301cd)
  of ghcjs-base), so you may need to recompile ghcjs and ghcjs-base.
* Add a function 'property' to create a property from any JSRef, not just Aeson values.
* Add a function 'elementProperty' to create a property from a ReactElementM, useful for
  interacting with foreign React classes.

# 0.9.3

* Don't require web-routes dependency if not building the routing example

# 0.9.2

* Bindings to react-bootstrap and the react addons
* Add new routing example application (thanks Vladimir Sekissov!)

# 0.9.1

* Switch to use the improved-base branch of ghcjs (thanks Vladimir Sekissov!)

# 0.9.0

* Initial release

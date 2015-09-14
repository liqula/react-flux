# 1.0.0

* Bindings to react-intl (http://formatjs.io/react/) for i18n support

* The type of `callback` has extended to allow arbitrary function properties to be
  passed to foreign classes.  The old `callback` accepted callbacks of type `Aeson.Value -> handler`
  while the new callback allows functions of any number of arguments, as long as each argument implements
  `FromJSRef`.  Since `Aeson.Value` implements `FromJSRef`, any existing calls to `callback` should still work.
  This change also caused some changes to types in `React.Flux.Internal`.

* Support for React 0.14
    * React 0.13 and 0.14 are both supported from the same Haskell code, the differences are handled internally.
    * If you are using React 0.14, you will have to include `react-dom.min.js` and make sure the
      `ReactDOM` variable is protected by closure similar to how `React` must be protected.
    * `initializeTouchEvents` has been removed from React 0.14, so you can remove the call from your app.
    * The SVG `image_` tag is now supported by `React.Flux.DOM`.
    * The new media events on images and videos don't have direct Haskell equivalents, instead the handlers can be
      created by the new `on` function in `React.Flux.PropertiesAndEvents`.
    * The CSS transitions in `React.Flux.Addons.React` were made simpler by just passing the raw
      properties.  There were several changes to the possible properties and covering them all from Haskell is not worth
      it when the properties can easily be created directly.

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

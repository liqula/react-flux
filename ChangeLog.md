# Unreleased

* React-flux has used the shouldComponentUpdate lifetime method to prevent re-rendering in some cases,
  but it was finicky and sometimes wouldn't work (not a correctness bug, just a missed performance
  improvement).  I now better understand when it works and does not work, and edited the documentation
  to explain this and what you can do in your own code (primarily add bang patterns to force thunks).
  The new documentation is at the end of the main module page.

* The shouldComponentUpdate lifetime method now knows about view props which are tuples of size two or
  three, and will be able to skip re-rendering as long as each component of the tuple is unchanged
  (and is not a thunk).

# 1.0.7

* Fix the build when building with GHC instead of GHCJS (an import was incorrectly protected by CPP)

# 1.0.6

* Fix a rare bug in stateful view event handlers: occasionally deepseq was not called and so there
  was a race between react reusing the event and the haskell code extracting data from the event.
* Add overlapping pragmas to the Callback instances, making the `callback` function easier to use
  without requiring explicit type signatures.  This change is backwards compatible.
* Some minor documentation updates.

# 1.0.5

* Fix a bug in the `jsonAjax` implementation: the request body was not properly JSON encoded.

# 1.0.4

* Add helper functions `ajax` and `jsonAjax` to `React.Flux.Combinators` to assist with sending
  a request to the backend and turning the response into actions.  See the haddocks for more
  information.

* Everything in react-flux is working with no changes against React 15 release candidate 1, although
  react-bootstrap is currently not compatible (https://github.com/react-bootstrap/react-bootstrap/issues/1686).

* Everything in `React.Flux.Addons.Intl` is working with no changes with react-intl 2.0 release candidate 1.

# 1.0.3

* I switched to using stack to build, and updated the documentation to explain how to use stack.  It is
  still possible to use cabal but I suggest using stack.  There is no functionality change in this release,
  just updated documentation.

# 1.0.2

* Update to build with latest ghcjs master.  The breaking change was
  https://github.com/ghcjs/ghcjs-base/commit/968dff527c2be2d3d4815e437ad9b2931ea1f35d
  which renamed JSRef to JSVal.  Therefore, react-flux no longer builds with ghcjs versions without
  this commit.

# 1.0.1

* Add formatting support for properties to `React.Flux.Addons.Intl`.  These are needed for example to translate
  the placeholder text for an input element.  This improvement caused a few changes to the types the Internal module.

* Add a new example [purecss-side-menu](https://bitbucket.org/wuzzeb/react-flux/src/tip/example/purecss-side-menu)
  showing a responsive side menu built with PureCSS.

* Add `classNames` function to `React.Flux.PropertiesAndEvents` to allow easily setting class names
  based on calculations.

* Add a new module `React.Flux.Combinators` which is re-exported by `React.Flux`.  The `Combinators` module
  contains useful utilities that while not required, make your life a little simpler.

# 1.0.0

* Bindings to react-intl (http://formatjs.io/react/) for i18n support.  This is useful even if your app is
  a single language, as it allows easy number, date, relative time, and message formatting like pluralization.
  It also supports multiple locales and translations of messages.

* The type of `callback` has extended to allow arbitrary function properties to be
  passed to foreign classes.  The old `callback` accepted callbacks of type `Aeson.Value -> handler`
  while the new callback allows functions of any number of arguments, as long as each argument implements
  `FromJSVal`.  Since `Aeson.Value` implements `FromJSVal`, any existing calls to `callback` should still work.
  This change also caused some changes to types in `React.Flux.Internal`.

* Add a function `nestedProperty` to `React.Flux.PropertiesAndEvents` to create nested properties.

* Support for React 0.14
    * React 0.13 and 0.14 are both supported from the same Haskell code, the differences are handled internally.
    * If you are using React 0.14, you will have to include `react-dom.min.js` and make sure the
      `ReactDOM` variable is protected by closure similar to how `React` must be protected.
    * `initializeTouchEvents` has been removed from React 0.14, so you can remove the call from your app.
    * The SVG `image_` tag is now supported by `React.Flux.DOM`.
    * The new media events on images and videos don't have direct Haskell equivalents, instead the handlers can be
      created by the new `on` function in `React.Flux.PropertiesAndEvents`.
    * The CSS transitions in `React.Flux.Addons.React` were made simpler by just passing the raw
      properties.  There were several changes to the possible properties in React 0.14 and covering them all
      from Haskell is not worth it when the properties can easily be created directly.

* `reactRenderToString` was added to allow executing a react-flux application using node.

# 0.9.4

* Fix to build with latest ghcjs-base (requires at least aaa4d59117f37d1b9c60a154a9128b3bcc6301cd)
  of ghcjs-base), so you may need to recompile ghcjs and ghcjs-base.
* Add a function 'property' to create a property from any JSVal, not just Aeson values.
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

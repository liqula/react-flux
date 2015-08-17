This directory contains an example TODO application.  The design is copied pretty much exactly from the
[flux todo example](https://github.com/facebook/flux/tree/master/examples/flux-todomvc).  It uses
the same actions, same views, and produces the same DOM, so the design overview from the flux
repository covers this example application as well.

When reading the code for the example application, you should start with `TodoStore.hs`.  Next, look
at `TodoDispatcher.hs` and `TodoViews.hs`.  Finally, you can look at `TodoComponents.hs` and
`Main.hs`.

# Build

To build, you must pass the `-fexample` flag to cabal.

~~~
cabal configure -fexample
cabal build
~~~

The result of this build is a file `dist/build/todo/todo.jsexe/all.js`.  There is a file
`example/todo-dev.html` which loads this `all.js` file directly from the `dist` directory, so you
can open `todo-dev.html` after building.

But to deploy a react-flux application, you should minimize it since the size of `all.js` is 1.8
mebibytes.  To do so, there is a `Makefile` which calls closure.  So if you have closure installed
on your path, you can execute

~~~
cd example
make
~~~

This produces a file `js-build/todo.min.js` which is only 500 kibibytes which when compressed with
gzip is 124 kibibytes.

# Testing

Finally, you might be interested to look at
[test/spec/TodoSpec.hs](https://bitbucket.org/wuzzeb/react-flux/src/tip/test/spec/TodoSpec.hs) as it
contains an [hspec-webdriver](https://hackage.haskell.org/package/hspec-webdriver) spec for the TODO
example application.

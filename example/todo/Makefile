js-build/todo.min.js: js-build/todo.js
	closure --compilation_level=ADVANCED_OPTIMIZATIONS js-build/todo.js > js-build/todo.min.js

js-build/todo.js: ../../dist/build/todo/todo.jsexe/all.js
	mkdir -p js-build
	echo "(function(global,React) {" > js-build/todo.js
	cat ../../dist/build/todo/todo.jsexe/all.js >> js-build/todo.js
	echo "})(this, window['React']);" >> js-build/todo.js
	sed -i 's/goog.provide.*//' js-build/todo.js
	sed -i 's/goog.require.*//' js-build/todo.js
	# https://github.com/ghcjs/shims/issues/21
	sed -i 's/final\([^a-z]\)/final0\1/g' js-build/todo.js

clean:
	rm -rf js-build

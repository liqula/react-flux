js-build/todo.min.js: js-build/todo.js
	closure --compilation_level=ADVANCED_OPTIMIZATIONS js-build/todo.js > js-build/todo.min.js

js-build/todo.js: ../../dist/build/todo/todo.jsexe/all.js
	mkdir -p js-build
	echo "(function(global,React,ReactDOM) {" > js-build/todo.js
	cat ../../dist/build/todo/todo.jsexe/all.js >> js-build/todo.js
	echo "})(window, window['React'], window['ReactDOM']);" >> js-build/todo.js
	sed -i 's/goog.provide.*//' js-build/todo.js
	sed -i 's/goog.require.*//' js-build/todo.js

clean:
	rm -rf js-build

todo.min.js: todo.js
	closure --compilation_level=ADVANCED_OPTIMIZATIONS todo.js > todo.min.js

todo.js: dist/build/todo/todo.jsexe/all.js
	echo "(function(global,React) {" > todo.js
	cat dist/build/todo/todo.jsexe/all.js >> todo.js
	echo "})(this, window['React']);" >> todo.js
	sed -i 's/goog.provide.*//' todo.js
	sed -i 's/goog.require.*//' todo.js

clean:
	rm -f todo.js todo.min.js

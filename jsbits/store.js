function hsreact$transform_store(store, newData) {
    var oldD = store.sdata;
    store.sdata = newData;
    h$release(oldD);
    store.views.map(function(f) { f(store.sdata); });
}

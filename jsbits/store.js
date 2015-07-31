/* As part of each store, we have a javascript object with two properties:

sdata: holds a value of type @Export storeData@ which is the current data for the store

views: an array of @setState@ functions for component views.  The component views
    add and remove from this property directly inside their lifecycle methods.
*/
function hsreact$transform_store(store, newData) {
    var oldD = store.sdata;
    store.sdata = newData;
    h$release(oldD);
    store.views.map(function(f) { f(store.sdata); });
}

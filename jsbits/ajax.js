/* jshint sub:true */

var hsreact$ajaxCallback;

function hsreact$setAjaxCallback(c) {
    hsreact$ajaxCallback = c;
}

function hsreact$ajax(req, handler) {
    var xhr = new XMLHttpRequest();
    xhr['open'](req['reqMethod'], req['reqURI']);
    var lst = req['reqHeaders'];
    for (var i = 0; i < lst.length; i++) {
        xhr['setRequestHeader'](lst[i][0], lst[i][1]);
    }
    xhr['onreadystatechange'] = function() {
        if (xhr['readyState'] === XMLHttpRequest['DONE']) {
            hsreact$ajaxCallback(xhr, {hs: handler});
        }
    };
    xhr['send'](req['reqBody']);
}

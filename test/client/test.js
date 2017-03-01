window.hsreact$log_when_updated = React['createClass']({
    'displayName': "log-when-updated",
    'componentWillUpdate': function() {
        hsreact$log_message("Update: " + this['props']['message']);
        console.log("Update: " + this['props']['message']);
    },
    'render': function() {
        return React['createElement']('span', {}, this['props']['message']);
    }
});

function hsreact$log_message(m) {
  if (!window.test_client_output) window.test_client_output = [];
  window.test_client_output.push(x);
}

window.hsreact$callback_wrapper = React['createClass']({
    'displayName':'callback wrapper',
    'render': function() {
        return React['createElement']('div', {}, [ React['createElement']('p', {'key': 'para'}, 'From Callback')
                                                 , React['createElement']('div', {'key': 'callback'},
                                                                          this['props']['foo'](5, 'Hello World'))
                                                 ]);
    }
});

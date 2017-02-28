window.hsreact$log_when_updated = React['createClass']({
    'displayName': "log-when-updated",
    'componentWillUpdate': function() {
        if (!window.test_client_output) window.test_client_output = [];
        window.test_client_output.push("Update: " + this['props']['message']);
        console.log("Update: " + this['props']['message']);
    },
    'render': function() {
        return React['createElement']('span', {}, this['props']['message']);
    }
});

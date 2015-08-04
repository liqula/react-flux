function hsreact$mk_ctrl_view(name, store, renderCb) {
    return React.createClass({ 
        displayName: name,
        getInitialState: function() { 
            return store.sdata; 
        }, 
        componentDidMount: function() { 
            store.views.push(this.setState); 
        }, 
        componentWillUnmount: function() {
            var idx = store.views.indexOf(this.setState); 
            if (idx >= 0) { store.views.splice(idx, 1); } 
            this._currentCallbacks.map(h$release);
            h$release(this.props.hs);
        }, 
        componentWillReceiveProps: function() { 
            h$release(this.props.hs); 
        }, 
        render: function() { 
            var arg = { 
                state: this.state, 
                props: this.props.hs, 
                newCallbacks: [], 
                elem:null, 
            }; 
            renderCb(arg); 
            this._currentCallbacks.map(h$release); 
            this._currentCallbacks = arg.newCallbacks; 
            return arg.elem; 
        }, 
        _currentCallbacks: [], 
    });
}

function hsreact$mk_view(name, renderCb) {
    return React.createClass({ 
        displayName: name,
        componentWillUnmount: function() {
            this._currentCallbacks.map(h$release);
            h$release(this.props.hs);
        }, 
        componentWillReceiveProps: function() {
            h$release(this.props.hs);
        }, 
        render: function() { 
            var arg = { 
                props: this.props.hs, 
                newCallbacks: [], 
                elem:null, 
            }; 
            renderCb(arg); 
            this._currentCallbacks.map(h$release); 
            this._currentCallbacks = arg.newCallbacks; 
            return arg.elem;
        }, 
        _currentCallbacks: [], 
    });
}

function hsreact$mk_class(name, initialState, renderCb) {
    return React.createClass({ 
        displayName: name,
        getInitialState: function() { 
            return { hs: initialState };
        }, 
        componentWillUnmount: function() {
            this._currentCallbacks.map(h$release);
            h$release(this.props.hs);
        }, 
        componentWillReceiveProps: function() { 
            h$release(this.props.hs); 
        }, 
        render: function() { 
            var that = this; 
            var arg = { 
                state: this.state.hs,
                props: this.props.hs,
                newCallbacks: [], 
                elem:null, 
                alterState: { 
                    getState: function() { return that.state.hs; }, 
                    setState: function(s) { that.setState({hs: s}); },
                }, 
            }; 
            renderCb(arg); 
            this._currentCallbacks.map(h$release); 
            this._currentCallbacks = arg.newCallbacks; 
            return arg.elem;
        }, 
        _currentCallbacks: [], 
    });
}

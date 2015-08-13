/* jshint sub:true */
function hsreact$mk_ctrl_view(name, store, renderCb) {
    return React['createClass']({
        'displayName': name,
        'getInitialState': function() {
            return store.sdata;
        },
        _onViewChange: function(x) { // allows binding to this
            this['setState'](x);
        },
        'shouldComponentUpdate': function(newProps, newState) {
            return this['props'].hs.root != newProps.hs.root || this['state'].root != newState.root;
        },
        'componentDidMount': function() {
            store.views.push(this._onViewChange);
        },
        'componentWillUnmount': function() {
            var idx = store.views.indexOf(this._onViewChange);
            if (idx >= 0) { store.views.splice(idx, 1); }
            this._currentCallbacks.map(h$release);
            h$release(this['props'].hs);
        },
        'componentWillReceiveProps': function() {
            h$release(this['props'].hs);
        },
        'render': function() {
            var arg = {
                state: this['state'],
                props: this['props'].hs,
                newCallbacks: [],
                elem:null
            };
            renderCb(arg);
            this._currentCallbacks.map(h$release);
            this._currentCallbacks = arg.newCallbacks;
            return arg.elem;
        },
        _currentCallbacks: []
    });
}

function hsreact$mk_view(name, renderCb) {
    return React['createClass']({
        'displayName': name,
        'componentWillUnmount': function() {
            this._currentCallbacks.map(h$release);
            h$release(this['props'].hs);
        },
        'shouldComponentUpdate': function(newProps, newState) {
            return this['props'].hs.root != newProps.hs.root;
        },
        'componentWillReceiveProps': function() {
            h$release(this['props'].hs);
        },
        'render': function() {
            var arg = {
                props: this['props'].hs,
                newCallbacks: [],
                elem:null
            };
            renderCb(arg);
            this._currentCallbacks.map(h$release);
            this._currentCallbacks = arg.newCallbacks;
            return arg.elem;
        },
        _currentCallbacks: []
    });
}

function hsreact$mk_stateful_view(name, initialState, renderCb) {
    return React['createClass']({
        'displayName': name,
        'getInitialState': function() {
            return { hs: initialState };
        },
        'shouldComponentUpdate': function(newProps, newState) {
            return this['props'].hs.root != newProps.hs.root || this['state'].hs.root != newState.hs.root;
        },
        'componentWillUnmount': function() {
            this._currentCallbacks.map(h$release);
            h$release(this['props'].hs);
            h$release(this['state'].hs);
        },
        'componentWillReceiveProps': function() {
            h$release(this['props'].hs);
        },
        'render': function() {
            var that = this;
            var arg = {
                state: this['state'].hs,
                props: this['props'].hs,
                newCallbacks: [],
                elem:null,
                alterState: {
                    getState: function() { return that['state'].hs; },
                    setState: function(s) {
                        h$release(that['state'].hs);
                        that['setState']({hs: s});
                    }
                }
            };
            renderCb(arg);
            this._currentCallbacks.map(h$release);
            this._currentCallbacks = arg.newCallbacks;
            return arg.elem;
        },
        _currentCallbacks: []
    });
}

function hsreact$mk_lifecycle_view(name, initialState, renderCb,
            willMountCb, didMountCb, willRecvPropsCb, willUpdateCb, didUpdateCb, willUnmountCb) {
    var cl = {
        'displayName': name,
        'getInitialState': function() {
            return { hs: initialState };
        },
        updateState: function(newState) {
            h$release(this['state'].hs);
            this['setState']({hs: newState});
        },
        'shouldComponentUpdate': function(newProps, newState) {
            return this['props'].hs.root != newProps.hs.root || this['state'].hs.root != newState.hs.root;
        },
        'componentWillReceiveProps': function(newProps) {
            try {
                if (willRecvPropsCb) {
                    willRecvPropsCb(this, newProps.hs);
                }
            } finally {
                h$release(this['props'].hs);
            }
        },
        'componentWillUnmount': function() {
            try {
                if (willUnmountCb) {
                    willUnmountCb(this);
                }
            } finally {
                this._currentCallbacks.map(h$release);
                h$release(this['props'].hs);
                h$release(this['state'].hs);
            }
        },
        'render': function() {
            var that = this;
            var arg = {
                state: this['state'].hs,
                props: this['props'].hs,
                newCallbacks: [],
                elem:null,
                alterState: {
                    getState: function() { return that['state'].hs; },
                    setState: function(s) {
                        h$release(that['state'].hs);
                        that['setState']({hs: s});
                    }
                }
            };
            renderCb(arg);
            this._currentCallbacks.map(h$release);
            this._currentCallbacks = arg.newCallbacks;
            return arg.elem;
        },
        _currentCallbacks: []
    };

    if (willUpdateCb) {
        cl['componentWillUpdate'] = function(nextProps, nextState) {
            willUpdateCb(this, {'props': nextProps, 'state': nextState});
        };
    }

    if (didUpdateCb) {
        cl['componentDidUpdate'] = function(oldProps, oldState) {
            didUpdateCb(this, {'props': oldProps, 'state': oldState});
        };
    }

    if (willMountCb) {
        cl['componentWillMount'] = function() {
            willMountCb(this);
        };
    }

    if (didMountCb) {
        cl['componentDidMount'] = function() {
            didMountCb(this);
        };
    }

    return React['createClass'](cl);
}

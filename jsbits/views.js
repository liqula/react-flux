/* jshint sub:true */

function hsreact$mk_class(name, renderCb, checkState, releaseState) {
    var cl = {
        'displayName': name,
        'componentWillReceiveProps': function() {
            h$release(this['props'].hs);
        },
        _updateAndReleaseState: function(s) {
            h$release(this['state'].hs);
            this['setState']({hs: s});
        },
        _updateState: function(s) {
            this['setState']({hs: s});
        },
        'componentWillUnmount': function() {
            this._currentCallbacks.map(h$release);
            h$release(this['props'].hs);
            if (releaseState) {
                h$release(this['state'].hs);
            }
        },
        'render': function() {
            var arg = {
                newCallbacks: [],
                elem:null
            };
            renderCb(this, arg);
            this._currentCallbacks.map(h$release);
            this._currentCallbacks = arg.newCallbacks;
            return arg.elem;
        },
        _currentCallbacks: []
    };
    if (checkState) {
        cl['shouldComponentUpdate'] = function(newProps, newState) {
            return this['props'].hs.root != newProps.hs.root || this['state'].hs.root != newState.hs.root;
        };
    } else {
        cl['shouldComponentUpdate'] = function(newProps, newState) {
            return this['props'].hs.root != newProps.hs.root;
        };
    }
    
    return cl;
}

function hsreact$mk_ctrl_view(name, store, renderCb) {
    var cl = hsreact$mk_class(name, renderCb, true, false);
    cl['getInitialState'] = function() {
        return {hs: store.sdata};
    };
    cl['componentDidMount'] = function() {
        store.views.push(this._updateState);
    };
    cl['componentWillUnmount'] = function() {
        var idx = store.views.indexOf(this._updateState);
        if (idx >= 0) { store.views.splice(idx, 1); }
        this._currentCallbacks.map(h$release);
        h$release(this['props'].hs);
    };
    return React['createClass'](cl);
}

function hsreact$mk_view(name, renderCb) {
    return React['createClass'](hsreact$mk_class(name, renderCb, false, false));
}

function hsreact$mk_stateful_view(name, initialState, renderCb) {
    var cl = hsreact$mk_class(name, renderCb, true, true);
    cl['getInitialState'] = function() {
        return { hs: initialState };
    };
    return React['createClass'](cl);
}

function hsreact$mk_lifecycle_view(name, initialState, renderCb,
            willMountCb, didMountCb, willRecvPropsCb, willUpdateCb, didUpdateCb, willUnmountCb) {
    var cl = hsreact$mk_class(name, renderCb, true, true);

    cl['getInitialState'] = function() {
        return { hs: initialState };
    };

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

    if (willRecvPropsCb) {
        cl['componentWillReceiveProps'] = function(newProps) {
            try {
                willRecvPropsCb(this, newProps.hs);
            } finally {
                h$release(this['props'].hs);
            }
        };
    }

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

    if (willUnmountCb) {
        cl['componentWillUnmount'] = function() {
            try {
                willUnmountCb(this);
            } finally {
                this._currentCallbacks.map(h$release);
                h$release(this['props'].hs);
                h$release(this['state'].hs);
            }
        };
    }

    return React['createClass'](cl);
}

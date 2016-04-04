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

    //Checks if the javascript representations of two haskell values are the same.
    //This can't check equality but just checks if the javascript object has not been
    //changed.  We have special support for tuples of size 2 and 3, where we do check if the individual components of
    //the tuple are equal.
    var areValuesSame = function(obj1, obj2) {
        if (obj1 == obj2) { //use two equal signs to test if the objects are the same
            return true;

        } else {

            //check tuples of size two and 3.
            //
            //Tuples of size 2 are stored as
            //  { d1: first value
            //  , d2: second value
            //  , f: constructor function
            //  }
            //
            //Tuples of size 3 are stored as
            //  { d1: first value
            //  , d2: { d1: second value
            //        , d2: third value
            //        }
            //  , f: constructor function
            //  }
            var obj1_f = (obj1['f'] || {})['name'];
            var obj2_f = (obj2['f'] || {})['name'];

            if (obj1_f === "h$ghczmprimZCGHCziTupleziZLz2cUZR_con_e" && obj1_f === obj2_f) {
                //pair
                return obj1['d1'] == obj2['d1'] && obj1['d2'] == obj2['d2']; //use two equal signs to test if the objects are the same

            } else if (obj1_f === "h$ghczmprimZCGHCziTupleziZLz2cUz2cUZR_con_e" && obj1_f === obj2_f) {
                var obj1_d2 = obj1['d2'] || {};
                var obj2_d2 = obj2['d2'] || {};
                return obj1['d1'] == obj2['d1'] && obj1_d2['d1'] == obj2_d2['d1'] && obj1_d2['d2'] == obj2_d2['d2'];
            } else {
                return false;
            }
        }
    };
    if (checkState) {
        cl['shouldComponentUpdate'] = function(newProps, newState) {
            return !areValuesSame(this['props'].hs.root, newProps.hs.root) || !areValuesSame(this['state'].hs.root, newState.hs.root);
        };
    } else {
        cl['shouldComponentUpdate'] = function(newProps, newState) {
            return !areValuesSame(this['props'].hs.root, newProps.hs.root);
        };
    }

    if (typeof ReactIntl != "undefined") {
        cl['contextTypes'] = {
            'intl': ReactIntl['intlShape']
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

//React 0.14 introduced React.Children.toArray.  Also, to be able to run template haskell splices,
//we need to defend againsg React not being defined.
var hsreact$children_to_array = typeof React !== "object" ? null : (React['Children']['toArray'] ? React['Children']['toArray'] :
    (function (children) {
        var ret = [];
        React['Children']['forEach'](children, function(x) {
            ret.push(x);
        });
        return ret;
    }));

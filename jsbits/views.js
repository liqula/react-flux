/* jshint sub:true */

function hsreact$mk_class(name, renderCb, checkState, releaseState, propsEq, stateEq) {
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
            try {
                return !propsEq(this['props'].hs, newProps.hs) || !stateEq(this['state'].hs, newState.hs);
            } catch(e) {
                console.log('propsEq or stateEq failed on component', name);
                throw e;
            }
        };
    } else {
        cl['shouldComponentUpdate'] = function(newProps, newState) {
            try {
                return !propsEq(this['props'].hs, newProps.hs);
            } catch(e) {
                console.log('propsEq failed on component', name);
                throw e;
            }
        };
    }

    if (typeof ReactIntl != "undefined") {
        cl['contextTypes'] = {
            'intl': ReactIntl['intlShape']
        };
    }

    return cl;
}

function hsreact$mk_lifecycle_view(name, initialState, renderCb,
            willMountCb, didMountCb, willRecvPropsCb, willUpdateCb, didUpdateCb, willUnmountCb, propsEq, stateEq) {
    var cl = hsreact$mk_class(name, renderCb, true, true, propsEq, stateEq);

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
//we need to defend against React not being defined.  Finally, this code may be evaluated *before*
//React is loaded, and since it's a function, we can defer referencing it until it's there.
var hsreact$children_to_array = function() {
    hsreact$children_to_array = (React['Children']['toArray'] ? React['Children']['toArray'] :
        (function (children) {
            var ret = [];
            React['Children']['forEach'](children, function(x) {
                ret.push(x);
            });
            return ret;
        }));
    return hsreact$children_to_array.apply(this, arguments);
};

function hsreact$check_ghcjs_obj_equal(x, y) {
    return x === y || (x.d1 && x.d1 === y.d1 && x.d2 === y.d2);
}

function hsreact$check_props_equal(newPropsI, oldPropsI) {
    var newProps = newPropsI.hs;
    var oldProps = oldPropsI.hs;
    if (newProps.length !== oldProps.length) return false;
    for (var i = 0; i < oldProps.length; i++) {
        if (!hsreact$check_ghcjs_obj_equal(newProps[i].root, oldProps[i].root))
            return false;
    }
    return true;
}

function hsreact$mk_new_class(name, renderCb) {
    var cl = {
        'displayName': name,
        'componentWillReceiveProps': function() {
            this['props'].hs.map(h$release);
        },
        'render': function() {
            var arg = {
                newCallbacks: [],
                elem: null
            };
            renderCb(this, arg);
            this._currentCallbacks.map(h$release);
            this._currentCallbacks = arg.newCallbacks;
            return arg.elem;
        },
        _currentCallbacks: []
    };

    if (typeof ReactIntl != "undefined") {
        cl['contextTypes'] = {
            'intl': ReactIntl['intlShape']
        };
    }

    return cl;
}

function hsreact$mk_new_view(name, renderCb, propsEq) {
    var cl = hsreact$mk_new_class(name, renderCb);
    cl['shouldComponentUpdate'] = function(newPropsI) {
        if (!hsreact$check_props_equal(newPropsI, this['props'])) return true;
        if (!propsEq(newPropsI, this['props'])) return true;
        return false;
    };
    cl['componentWillUnmount'] = function() {
        this._currentCallbacks.map(h$release);
        this['props'].hs.map(h$release);
    };
    return React['createClass'](cl);
}

function hsreact$mk_new_stateful_view(name, initialState, renderCb, propsEq, stateEq) {
    var cl = hsreact$mk_new_class(name, renderCb);
    cl['getInitialState'] = function() {
        return { hs: initialState };
    };
    cl._updateAndReleaseState = function(s) {
        h$release(this['state'].hs);
        this['setState']({hs: s});
    };
    cl['shouldComponentUpdate'] = function(newPropsI, newStateI) {
        if (!hsreact$check_props_equal(newPropsI, this['props'])) return true;
        if (!propsEq(newPropsI, this['props'])) return true;
        if (!hsreact$check_ghcjs_obj_equal(newStateI.hs.root, this['state'].hs.root)) return true;
        if (!stateEq(newStateI.hs, this['state'].hs)) return true;
        return false;
    };
    cl['componentWillUnmount'] = function() {
        this._currentCallbacks.map(h$release);
        h$release(this['state'].hs);
        this['props'].hs.map(h$release);
    };
    return React['createClass'](cl);
}

function hsreact$make_ctrl_view_callback(elem, artifact) {
    return function(newStoreData) {
        var newState = Object.assign({}, elem['state'].hs);
        artifact.forEach(function(st) {
            if (st.call) {
                var arg = {input: newStoreData, output: null};
                st.call(arg);
                newState[st.i] = arg.output;
            } else {
                newState[st.i] = newStoreData.root;
            }
        });
        elem['setState']({hs:newState});
    };
}

function hsreact$mk_new_ctrl_view(name, renderCb, artifacts, propsEq, statesEq) {
    var cl = hsreact$mk_new_class(name, renderCb);
    cl['shouldComponentUpdate'] = function(newPropsI, newStateI) {
        if (!hsreact$check_props_equal(newPropsI, this['props'])) return true;
        if (!propsEq(newPropsI.hs, this['props'].hs)) return true;
        var newState = newStateI.hs;
        var oldState = this['state'].hs;
        for (var k in newState) {
            if (!hsreact$check_ghcjs_obj_equal(newState[k], oldState[k])) return true;
            // TODO: statesEq (see comments in the beginning of src/React/Flux/ForeignEq.hs for context.)
        }
        return false;
    };
    cl['getInitialState'] = function() {
        var reactState = {};
        for (var storeTy in artifacts) {
            var store = hsreact$storedata[storeTy];
            artifacts[storeTy].forEach(function(st) {
                if (st.call) {
                    var arg = {input: store.sdata, output: null};
                    st.call(arg);
                    reactState[st.i] = arg.output;
                } else {
                    reactState[st.i] = store.sdata.root;
                }
            });
        };
        return {hs: reactState};
    };
    cl['componentDidMount'] = function() {
        this._hsreactViewCallbacks = {};
        for (var storeTy in artifacts) {
            var store = hsreact$storedata[storeTy];
            var artifact = artifacts[storeTy];
            this._hsreactViewCallbacks[storeTy] =
                hsreact$register_view(store, hsreact$make_ctrl_view_callback(this, artifact));
        };
    };
    cl['componentWillUnmount'] = function() {
        for (var storeTy in artifacts) {
            var store = hsreact$storedata[storeTy];
            hsreact$clear_view(store, this._hsreactViewCallbacks[storeTy]);
        }
        this._currentCallbacks.map(h$release);
        this['props'].hs.map(h$release);
    };
    return React['createClass'](cl);
}

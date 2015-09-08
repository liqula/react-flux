//Use a function so that closure will remove this if it isn't used.
function hsreact$intl_mixin_class() {
   return React['createClass']({
    'displayName': 'Haskell ReactIntl Locale Mixin',
    'mixins': [window['ReactIntl']['IntlMixin']],
    'render': function() { return this['props']['children']; }
    });
}

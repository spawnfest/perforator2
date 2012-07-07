// WARNING
// This file is a dirty file. However it has to remain so.
// It is the single one which uses the global variable called window.
// Also, it overwrites window.console.log to an empty handler, if a cookie called 'log' is not set to 'yes'

var cookie = require('cookie');

exports.d3 = window.d3;
exports.nv = window.nv;

exports.setCookie = function(name, value) {
    if(value === null) {
        var exp = new Date();
        exp.setDate(exp.getDate() - 1);
        window.document.cookie = cookie.serialize(name, '', {
            path : '/',
            expires : exp
        });
    } else {
        window.document.cookie = cookie.serialize(name, value, {
            path : '/'
        });
    }
};
exports.getCookie = function(name) {
    var cookies = cookie.parse(window.document.cookie);
    if(typeof cookies[name] === 'string') {
        return cookies[name];
    } else {
        return null;
    }
};
exports.getPath = function() {
    return window.location.pathname;
};
exports.createSocket = function() {
    return new window.WebSocket('ws://' + window.location.host + '/');
};
if(typeof window.console === 'undefined' || typeof window.console.log === 'undefined' || exports.getCookie('log') !== 'yes') {
    window.console = {
        log : function() {
        }
    };
}

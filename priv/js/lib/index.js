var t = require('./templates');
var qwery = require('qwery');
var domready = require('domready');
var bonzo = require('bonzo');
var p = require('page');
var log = require('./log');
var bean = require('bean');
var window = require('./window');
var step = require('step');

var socket = window.createSocket();
step(function() {
    domready(this.parallel());
    socket.onmessage = function(event) {
        event = JSON.parse(event.data.toString());
        bean.fire(socket, event.type, event.err, event.msg);
    };
    // TODO uncomment this when WebSocket server is ready
    //socket.onopen = this.parallel();
}, function() {
    var previousPath = null;
    var page = {
        once : function(event, handler) {
            bean.one(socket, event, function(err, msg) {
                console.log('page.once', event, err, msg);
                handler(err, msg);
            });
        },
        on : function(event, handler) {
            var handlerWrapper = function(err, msg) {
                console.log('page.on', event, err, msg);
                handler(err, msg);
            };
            bean.add(socket, event, handlerWrapper);
            return function() {
                bean.remove(socket, event, handlerWrapper);
            };
        },
        emit : function(event, err, msg) {
            console.log('page.emit', event, err, msg);
            socket.send(JSON.stringify({
                err : err,
                msg : msg,
                type : event
            }));
        },
        req : function(event, err, msg, cb) {
            page.once(event, cb);
            page.emit(event, err, msg);
        },
        body : bonzo(qwery('#body')[0]),
        handle : function(path, cb) {
            console.log('adding handler', path);
            p(path, function(ctx) {
                setTimeout(function() {
                    var nextPath = window.getPath();
                    if(previousPath !== nextPath) {
                        console.log('handling', previousPath, nextPath);
                        bean.fire(page, 'page', [previousPath, nextPath]);
                        cb(previousPath, nextPath, ctx.params);
                        previousPath = nextPath;
                    }
                }, 0);
            });
        },
        beforego : function(cb) {
            bean.one(page, 'page', cb);
        },
        go : function(path) {
            p(path);
        }
    };

    log.init(page, function() {
        p({
            click : true,
            popstate : true,
            dispatch : true
        });
        page.go(window.getPath());
    });
});

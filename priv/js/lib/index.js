var t = require('./templates');
var qwery = require('qwery');
var domready = require('domready');
var bonzo = require('bonzo');
var p = require('page');
var log = require('./log');
var run = require('./run');
var test = require('./test');
var projectEdit = require('./projectEdit');
var compare = require('./compare');
var bean = require('bean');
var window = require('./window');
var step = require('step');

var socket = window.createSocket();
step(function() {
    domready(this.parallel());
    socket.onmessage = function(event) {
        event = JSON.parse(event.data.toString());
        bean.fire(socket, event.type, [event.err, event.msg]);
    };
    socket.onopen = this.parallel();
}, function() {
    var previousPath = null;
    this(null, {
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
            this.once(event, cb);
            this.emit(event, err, msg);
        },
        body : bonzo(qwery('#body')[0]),
        handle : function(path, cb) {
            var self = this;
            console.log('adding handler', path);
            p(path, function(ctx) {
                setTimeout(function() {
                    var nextPath = window.getPath();
                    if(previousPath !== nextPath) {
                        console.log('handling', previousPath, nextPath);
                        bean.fire(self, 'page', [previousPath, nextPath]);
                        cb(previousPath, nextPath, ctx.params);
                        previousPath = nextPath;
                    }
                }, 0);
            });
        },
        beforego : function(cb) {
            bean.one(this, 'page', cb);
        },
        go : function(path) {
            p(path);
        }
    });
}, function(_, page) {
    this.parallel()(null, page);
    run.init(page, this.parallel());
    test.init(page, this.parallel());
    projectEdit.init(page, this.parallel());
    compare.init(page, this.parallel());
    // log should be initialized last, otherwise it could take over /project/*
    // url from projectEdit (/project/add)
    log.init(page, this.parallel());
}, function(_, page) {
    p({
        click : true,
        popstate : true,
        dispatch : true
    });
    page.go(window.getPath());
});

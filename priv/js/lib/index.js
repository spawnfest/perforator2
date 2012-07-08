var t = require('./templates');
var qwery = require('qwery');
var v = require('valentine');
var domready = require('domready');
var reqwest = require('reqwest');
var equal = require('deep-equal');
var bonzo = require('bonzo');
var p = require('page');
var log = require('./log');
var run = require('./run');
var test = require('./test');
var projectEdit = require('./projectEdit');
var serverEmulation = require('./serverEmulation');
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
        req : function(resource, msg, cb) {
            cb = cb || function(){};
            if(['project/new', 'project/update', 'project', 'projects', 'builders'].indexOf(resource) >= 0) {
                reqwest({
                    url : '/api/1/' + resource,
                    method : 'post',
                    type : 'json',
                    data : JSON.stringify(msg),
                    contentType: 'application/json',
                    error : function() {
                        console.log('req error', resource, msg, arguments);
                    },
                    success : function(resp) {
                        if(resp.err) {
                            cb({
                                err : resp.err,
                                msg : resp.msg
                            }, null);
                        } else {
                            cb(null, resp.msg);
                        }
                    }
                });
            } else {
                serverEmulation.post(resource, msg, cb);
            }
        },
        body : bonzo(qwery('#body')[0]),
        projectId : null,
        handle : function(path, cb) {
            var self = this;
            console.log('adding handler', path);
            p(path, function(ctx) {
                setTimeout(function() {
                    var nextPath = window.getPath();
                    if(previousPath !== nextPath) {
                        console.log('handling', previousPath, nextPath);
                        bean.fire(self, 'page', [previousPath, nextPath, ctx.params]);
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
    serverEmulation.init(page, this.parallel());
    var cb = this.parallel();

    var insertProject = function(projects, project) {
        for(var i = 0; i < projects.length; i += 1) {
            if(projects[i].name > project.name) {
                projects[i].splice(i, 0, project);
                return {
                    after : false,
                    project : projects[i + 1]
                };
            }
        }
        projects.push(project);
        return {
            after : projects.length === 1 ? null : true,
            project : projects[projects.length - 2]
        };
    };

    var replaceProject = function(projects, project) {
        for(var i = 0; i < projects.length; i += 1) {
            if(projects[i].id === project.id) {
                projects[i] = project;
                return;
            }
        }
    };

    step(function() {
        page.req('projects', null, this.parallel());
        page.req('builders', null, this.parallel());
    }, function(_, projects, workers) {
        page.on('workerPoolChanged', function(_, w) {
            workers = w;
            bonzo(qwery('.app-worker')).remove();
            var html = '';
            v.each(w, function(worker) {
                html += t.worker.render(worker);
            });
            bonzo(qwery('#sidebar')).append(html);
        });
        page.req('projects', null, function(_, projects) {
            var updateSidebar = function() {
                v.each(projects, function(p) {
                    if(page.projectId === p.id) {
                        p.opened = true;
                    } else {
                        p.opened = false;
                    }
                });
                bonzo(qwery('#sidebar')).html(t.sidebar.render({
                    projectId : page.projectId,
                    projects : projects,
                    workers : workers
                }, t));
            };
            bean.add(page, 'page', function(from, to, params) {
                if(params.length > 0) {
                    var projectId = parseInt(params[0], 10);
                    if(projectId !== page.projectId) {
                        page.projectId = projectId;
                        updateSidebar();
                    }
                    params.shift();
                }
            });
            updateSidebar();
            cb(null);
            bean.add(page, 'projectUpdated', function(project) {
                console.log('projectUpdated', project);
                replaceProject(projects, project);
                if(project.id === page.projectId) {
                    project.opened = true;
                }
                bonzo(qwery('#project-' + project.id)).replaceWith(t.project.render(project));
            });
            bean.add(page, 'projectAdded', function(project) {
                console.log('projectAdded', project);
                if(project.id === page.projectId) {
                    project.opened = true;
                }
                var position = insertProject(projects, project);
                var html = t.project.render(project);
                if(position.after === null) {
                    bonzo(qwery('#projects-header')).after(html);
                } else {
                    if(position.after) {
                        bonzo(qwery('#project-' + position.project.id)).after(html);
                    } else {
                        bonzo(qwery('#project-' + position.project.id)).before(html);
                    }
                }
            });
        });
    });
}, function(_, page) {
    p({
        click : true,
        popstate : true,
        dispatch : true
    });
    page.go(window.getPath());
});

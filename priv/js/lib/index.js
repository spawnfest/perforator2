var t = require('./templates');
var common = require('./common');
var qwery = require('qwery');
var v = require('valentine');
var domready = require('domready');
var reqwest = require('reqwest');
var equal = require('deep-equal');
var deep = require('./deep');
var bonzo = require('bonzo');
var p = require('page');
var log = require('./log');
var run = require('./run');
var test = require('./test');
var projectEdit = require('./projectEdit');
var serverEmulation = require('./serverEmulation');
var compare = require('./compare');
var bean = require('bean');
var w = require('./window');
var step = require('step');

var socket = w.createSocket();
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
        store : {
            builders : []
        },
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
            console.log('page.req', resource, msg);
            cb = cb || function(){};
            if(['build_now', 'project/new', 'project/update', 'project', 'projects', 'builders', 'build'].indexOf(resource) >= 0) {
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
                        console.log('resp', resp);
                        if(resp.err) {
                            cb(resp, null);
                        } else {
                            cb(null, resp.msg);
                        }
                    }
                });
            } else {
                serverEmulation.post(resource, msg, cb);
            }
        },
        body : w.el('body'),
        projectId : null,
        handle : function(path, cb) {
            var self = this;
            console.log('adding handler', path);
            p(path, function(ctx) {
                setTimeout(function() {
                    var nextPath = w.getPath();
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
    bean.add(page, 'page', function(from, to, params) {
        if(params.length > 0) {
            var projectId = parseInt(params[0], 10);
            if(projectId !== page.projectId) {
                page.projectId = projectId;
                bean.fire(page, 'projectId');
            }
            params.shift();
        }
    });
    bean.add(page.store.builders, 'refresh', function() {
        page.req('builders', null, function(_, builders) {
            var changes = deep.update(page.store.builders, builders);
            if(changes.updated.length > 0 || changes.inserted.length > 0 || changes.deleted.length > 0) {
                bean.fire(page.store.builders, 'change', changes);
            }
        });
    });
    bean.fire(page.store.builders, 'refresh');
    page.on('queue_size', function(_, queue_size) {
        var q = common.findBy(page.store.builders, 'name', queue_size.name);
        q.queue_size = queue_size.queue_size;
        bean.fire(q, 'update');
    });
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

    page.req('projects', null, function(_, projects) {
        var updateSidebar = function() {
            v.each(projects, function(p) {
                if(page.projectId === p.id) {
                    p.opened = true;
                } else {
                    p.opened = false;
                }
            });
            w.el('sidebar').html(t.sidebar.render({
                projectId : page.projectId,
                projects : projects,
                workers : page.store.builders
            }, t));
        };
        bean.add(page, 'projectId', updateSidebar);
        updateSidebar();
        bean.add(w.el('build-now')[0], 'click', function(e) {
            page.req('build_now', page.projectId);
            e.preventDefault();
        });
        var addBuilderListener = function(builder) {
            var onUpdate = function() {
                w.el('builder-li-' + builder.name).replaceWith(t.worker.render(builder));
            };
            var onDelete = function() {
                bean.remove(builder, 'update', onUpdate);
                bean.remove(builder, 'delete', onDelete);
            };
            bean.add(builder, 'update', onUpdate);
            bean.add(builder, 'delete', onDelete);
        };
        bean.add(page.store.builders, 'change', function(changes) {
            bonzo(qwery('.app-worker')).remove();
            var html = '';
            v.each(page.store.builders, function(builder) {
                html += t.worker.render(builder);
            });
            v.each(changes.inserted, addBuilderListener);
            w.el('sidebar').append(html);
        });
        v.each(page.store.builders, addBuilderListener);
        cb(null);
        bean.add(page, 'projectUpdated', function(project) {
            replaceProject(projects, project);
            if(project.id === page.projectId) {
                project.opened = true;
            }
            w.el('project-' + project.id).replaceWith(t.project.render(project));
        });
        bean.add(page, 'projectAdded', function(project) {
            if(project.id === page.projectId) {
                project.opened = true;
            }
            var position = insertProject(projects, project);
            var html = t.project.render(project);
            if(position.after === null) {
                w.el('projects-header').after(html);
            } else {
                if(position.after) {
                    w.el('project-' + position.project.id).after(html);
                } else {
                    w.el('project-' + position.project.id).before(html);
                }
            }
        });
    });
    p({
        click : true,
        popstate : true,
        dispatch : true
    });
    page.go(w.getPath());
});

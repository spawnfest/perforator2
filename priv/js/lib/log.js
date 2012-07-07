var t = require('./templates');
var v = require('valentine');
var qwery = require('qwery');
var step = require('step');
var bean = require('bean');
var bonzo = require('bonzo');
var moment = require('moment');

var insertProject = function(projects, project) {
    for(var i = 0; i < projects.length; i += 1) {
        if(projects[i].title > project.title) {
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

exports.init = function(page, cb) {
    step(function() {
        page.req('projects', null, null, this);
    }, function(_, projects) {
        page.on('projectUpdated', function(_, project) {
            replaceProject(projects, project);
            bonzo(qwery('#project-' + project.id)).replaceWith(t.project.render(project));
        });
        page.on('projectAdded', function(_, project) {
            // ABSTRACT THIS SHIT
            var position = insertProject(projects, project);
            var html = t.project.render(project);
            if(position.after === null) {
                bonzo(qwery('#projects-header')).append(html);
            } else {
                if(position.after) {
                    bonzo(qwery('#project-' + position.project.id)).after(html);
                } else {
                    bonzo(qwery('#project-' + position.project.id)).before(html);
                }
            }
        });
        page.handle('/', function() {
            page.go('/project/3ttat');
        });
        page.handle(/^\/project\/(.+)$/, function(from, to, params) {
            step(function() {
                page.req('runs', null, params[0], this);
            }, function(_, res) {
                var offProjectUpdated = page.on('projectUpdated', function(_, p) {
                    if(p.id === project.id) {
                        bonzo(qwery('#project-header')).text(project.title);
                    }
                });
                page.beforego(function(from, to) {
                    offProjectUpdated();
                });
                var runs = res.runs;
                var project = null;
                var runLag = null;

                runs.reverse();
                v.each(runs, function(run) {
                    if(runLag === null) {
                        run.timeDelta = null;
                    } else {
                        run.timeDelta = run.time - runLag.time;
                    }
                    runLag = run;
                });
                runs.reverse();

                v.each(projects, function(p) {
                    if(p.id === params[0]) {
                        project = p;
                        p.opened = true;
                    } else {
                        p.opened = false;
                    }
                });
                v.each(runs, function(run) {
                    run.started = moment(new Date(run.started)).fromNow();
                });
                console.log(projects);
                page.body.html(t.log.render({
                    projects : projects,
                    runs : runs,
                    project : project
                }, t));
                v.each(qwery('tr'), function(row) {
                    bean.add(row, 'click', function() {
                        page.go('/run/' + bonzo(row).data('id'));
                    });
                });
            });
        });
        cb();
    });
};

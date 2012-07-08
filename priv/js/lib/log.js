var t = require('./templates');
var v = require('valentine');
var qwery = require('qwery');
var step = require('step');
var bean = require('bean');
var bonzo = require('bonzo');
var moment = require('moment');

exports.init = function(page, cb) {
    page.handle('/', function() {
        page.go('/1');
    });
    page.handle(/^\/(.+)$/, function(from, to, params) {
        step(function() {
            page.req('runs', page.projectId, this.parallel());
            page.req('project', page.projectId, this.parallel());
        }, function(_, runs, project) {
            var offs = [];
            offs.push(page.on('projectUpdated', function(_, p) {
                if(p.id === project.id) {
                    bonzo(qwery('#project-header')).text(project.name);
                }
            }));
            page.beforego(function(from, to) {
                v.each(offs, function(off) {
                    off();
                });
            });
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

            v.each(runs, function(run) {
                run.started = moment(new Date(run.started)).fromNow();
            });
            page.body.html(t.log.render({
                runs : runs,
                project : project
            }, t));
            v.each(qwery('tr'), function(row) {
                bean.add(row, 'click', function() {
                    page.go('/' + project.id + '/run/' + bonzo(row).data('id'));
                });
            });
        });
    });
    cb();
};

var t = require('./templates');
var v = require('valentine');
var qwery = require('qwery');
var step = require('step');
var bean = require('bean');
var bonzo = require('bonzo');
var moment = require('moment');
var common = require('./common');

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
            offs.push(page.on('build_finished', function(_, buildFinished) {
                if(buildFinished.project_id === project.id) {
                    var run = common.findById(runs, buildFinished.build_id);
                    run.finished = true;
                    run.time = (buildFinished.timestamp - run.buildInit.timestamp) * 1000;
                    run.timeDelta = run.time - run.previous.time;
                    bonzo(qwery('#run-' + buildFinished.build_id)).replaceWith(t.logRun.render(run, t));
                }
            }));
            offs.push(page.on('build_init', function(_, buildInit) {
                if(buildInit.project_id === project.id) {
                    var run = {
                        previous : runs[0],
                        buildInit : buildInit,
                        id : buildInit.build_id,
                        started : moment(new Date(buildInit.timestamp * 1000)).format('LLLL'),
                        finished : false,
                        modules : '',
                        tests : ''
                    };
                    runsEl.prepend(t.logRun.render(run, t));
                    runs.unshift(run);
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
                run.finished = true;
                if(runLag === null) {
                    run.timeDelta = null;
                } else {
                    run.timeDelta = run.time - runLag.time;
                }
                runLag = run;
            });
            runs.reverse();

            v.each(runs, function(run) {
                run.started = moment(new Date(run.started)).format('LLLL');
            });
            page.body.html(t.log.render({
                runs : runs,
                project : project
            }, t));
            var runsEl = bonzo(qwery('#runs'));
            v.each(qwery('tr'), function(row) {
                bean.add(row, 'click', function() {
                    page.go('/' + project.id + '/run/' + bonzo(row).data('id'));
                });
            });
        });
    });
    cb();
};

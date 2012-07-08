var t = require('./templates');
var w = require('./window');
var v = require('valentine');
var qwery = require('qwery');
var step = require('step');
var bean = require('bean');
var bonzo = require('bonzo');
var moment = require('moment');
var common = require('./common');

exports.init = function(page, cb) {
    page.handle('/', function() {
        if(page.projectId) {
            page.go('/' + page.projectId);
        } else {
            page.go('/1');
        }
    });
    page.handle(/^\/(.+)$/, function(from, to, params) {
        step(function() {
            page.req('builds', page.projectId, this.parallel());
            page.req('project', page.projectId, this.parallel());
        }, function(_, runs, project) {
            var offs = [];
            offs.push(page.on('build_finished', function(_, buildFinished) {
                if(buildFinished.project_id === project.id) {
                    var run = common.findById(runs, buildFinished.build_id);
                    run.finished = true;
                    run.succeeded = buildFinished.success;
                    run.time = (buildFinished.timestamp - run.buildInit.timestamp) * 1000;
                    if(run.previous) {
                        run.timeDelta = run.time - run.previous.time;
                    }
                    w.el('run-' + run.id).replaceWith(t.logRun.render(run, t));
                    attachClickHandler(w.el('run-' + run.id)[0]);
                }
            }));
            offs.push(page.on('build_init', function(_, buildInit) {
                if(buildInit.project_id === project.id) {
                    var run = {
                        previous : runs[0] || null,
                        buildInit : buildInit,
                        id : buildInit.build_id,
                        commit_id : buildInit.commit_id,
                        started : moment(new Date(buildInit.timestamp * 1000)).format('LLLL'),
                        finished : false,
                        modules : '',
                        tests : ''
                    };
                    runsEl.prepend(t.logRun.render(run, t));
                    attachClickHandler(w.el('run-' + run.id)[0]);
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
                if(runLag === null) {
                    run.timeDelta = null;
                } else {
                    run.timeDelta = run.time - runLag.time;
                }
                runLag = run;
            });
            runs.reverse();

            v.each(runs, function(run) {
                run.started = moment(new Date(run.started * 1000)).format('LLLL');
            });
            page.body.html(t.log.render({
                runs : runs,
                project : project
            }, t));
            var runsEl = bonzo(qwery('#runs'));
            var attachClickHandler = function(row) {
                if(row.parentNode.nodeName.toLowerCase() === 'thead') {
                    return;
                }
                bean.add(row, 'click', function() {
                    page.go('/' + project.id + '/build/' + bonzo(row).data('id'));
                });
            };
            v.each(qwery('tr'), attachClickHandler);
        });
    });
    cb();
};

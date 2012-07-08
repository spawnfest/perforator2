var t = require('./templates');
var common = require('./common');
var moment = require('moment');
var bean = require('bean');
var step = require('step');
var bonzo = require('bonzo');
var qwery = require('qwery');
var w = require('./window');
var v = require('valentine');
var series = require('./series');
series = series.series;

exports.init = function(page, cb) {
    page.handle(/^\/(.+)\/build\/(.+)$/, function(from, to, params) {
        var buildId = parseInt(params[0], 10);
        step(function() {
            page.req('previous_build', buildId, this);
        }, function(err, previousBuildId) {
            if(previousBuildId) {
                page.req('build', previousBuildId, this);
            } else {
                this(null, null);
            }
        }, function(err, previousBuild) {
            page.req('build', buildId, this.parallel());
            this.parallel()(null, previousBuild);
        }, function(err, modules, previousModules) {
            console.log('rendering build', modules, previousModules);
            if(err) {
                page.body.html(t.error.render({
                    title : 'Build #' + buildId + ' did not succeed',
                    message : err.err,
                    details : err.msg
                }));
                return;
            }
            v.each(modules, function(module) {
                v.each(module.test_cases, function(test) {
                    test.id = 'test-' + module.name + '-' + test.name;
                });
            });
            var seriesMap = {};
            v.each(series, function(series) {
                seriesMap[series.key] = series;
            });
            page.body.html(t.run.render({
                modules : modules,
                series : series,
                id : buildId
            }));
            var select = bonzo(qwery('select'));
            bean.add(select[0], 'change', function() {
                setSeries(select.val());
            });
            var chart = w.nv.models.bulletChart();
            v.each(modules, function(module) {
                v.each(module.test_cases, function(test) {
                    bean.add(qwery('#' + test.id)[0].parentNode.parentNode, 'click', function() {
                        page.go('/' + page.projectId + '/test/' + module.name + '/' + test.name);
                    });
                });
            });
            var currentSeries = null;
            var setSeries = function(key) {
                currentSeries = key;
                v.each(modules, function(module) {
                    var previousModule = previousModules ? common.findBy(previousModules, 'name', module.name) : null;
                    console.log('previousModule', module.name);
                    v.each(module.test_cases, function(test) {
                        var previousTest = previousModule ? common.findBy(previousModule.test_cases, 'name', test.name) : null;
                        console.log('previousTest', test.name);
                        var id = 'test-' + module.name + '-' + test.name;
                        w.el(id).empty();
                        if(!test.successful) {
                            bonzo(w.el(id)[0].parentNode).html('<p>' + test.name + ' did not succeed.</p>');
                            return;
                        }
                        var data = test.result[key];

                        // TODO make it real
                        data.previous = previousTest && previousTest.successful ? previousTest.result[key] : data;

                        var ranges = [data.min, data.mean, data.max];
                        w.d3.select('#' + id).datum({
                            title: test.name,
                            subtitle: seriesMap[key].units,
                            ranges: ranges,
                            measures: [data.mean],
                            markers: [data.previous.mean]
                        }).call(chart);
                        bonzo(qwery('#' + id + ' .measure')).css('fill', data.mean > data.previous.mean ? (seriesMap[key].higherBetter ? '#0f0' : '#f00') : (seriesMap[key].higherBetter ? '#f00' : '#0f0'));
                    });
                });
            };
            setSeries(series[0].key);
            w.nv.utils.windowResize(function() {
                setSeries(currentSeries);
            });
        });
    });
    cb();
};

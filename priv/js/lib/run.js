var t = require('./templates');
var moment = require('moment');
var bean = require('bean');
var bonzo = require('bonzo');
var qwery = require('qwery');
var w = require('./window');
var v = require('valentine');
var series = require('./series');
series = series.series;

exports.init = function(page, cb) {
    page.handle(/^\/(.+)\/run\/(.+)$/, function(from, to, params) {
        var state = {
            projectId : params[0],
            id : params[1]
        };
        page.req('run', state, function(_, modules) {
            v.each(modules, function(module) {
                v.each(module.tests, function(test) {
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
                id : state.id
            }));
            var select = bonzo(qwery('select'));
            bean.add(select[0], 'change', function() {
                setSeries(select.val());
            });
            var chart = w.nv.models.bulletChart();
            v.each(modules, function(module) {
                v.each(module.tests, function(test) {
                    bean.add(qwery('#' + test.id)[0].parentNode.parentNode, 'click', function() {
                        page.go('/' + state.projectId + '/test/' + module.name + '/' + test.name);
                    });
                });
            });
            var currentSeries = null;
            var setSeries = function(key) {
                currentSeries = key;
                v.each(modules, function(module) {
                    v.each(module.tests, function(test) {
                        bonzo(qwery('#' + test.id)).empty();
                        var data = test.series[key];
                        var ranges = [];
                        ranges.push(typeof data.min === 'undefined' ? 0 : data.min);
                        if(typeof data.mean !== 'undefined') {
                            ranges.push(data.mean);
                        }
                        ranges.push(typeof data.max === 'undefined' ? Math.max(data.current, data.previous) : data.max);
                        w.d3.select('#' + test.id).datum({
                            title: test.name,
                            subtitle: seriesMap[key].units,
                            ranges: ranges,
                            measures: [data.current],
                            markers: [data.previous]
                        }).call(chart);
                        bonzo(qwery('#' + test.id + ' .measure')).css('fill', test.series[key].current > test.series[key].previous ? (seriesMap[key].higherBetter ? '#0f0' : '#f00') : (seriesMap[key].higherBetter ? '#f00' : '#0f0'));
                    });
                });
            };
            setSeries('time');
            w.nv.utils.windowResize(function() {
                setSeries(currentSeries);
            });
        });
    });
    cb();
};

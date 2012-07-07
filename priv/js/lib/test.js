var v = require('valentine');
var qwery = require('qwery');
var bonzo = require('bonzo');
var bean = require('bean');
var t = require('./templates');
var w = require('./window');
var series = require('./series');
series = series.series;

exports.init = function(page, cb) {
    page.handle(/^\/test\/(.+)\/(.+)$/, function(from, to, params) {
        var moduleName = params[0];
        var testName = params[1];
        var runs = [
            {
                name : 'abcd64d7',
                time : 92000,
                load : 1,
                memutil : 122000000,
                cpuutil : 0.9
            }, {
                name : 'ab235799',
                time : 72000,
                load : 0.7,
                memutil : 922000000,
                cpuutil : 0.4
            }, {
                name : '32asdf55',
                time : 78000,
                load : 1.2,
                memutil : 102000000,
                cpuutil : 1.0
            }
        ];
        v.each(series, function(series) {
            var lag = null;
            var key = series.key;
            v.each(runs, function(run) {
                if(lag === null || run[key] === null) {
                    if(run[key] === null) {
                        run[key + '_label'] = '-';
                    } else {
                        run[key + '_label'] = run[key] + series.unitsShort;
                    }
                    run[key + '_class'] = '';
                } else {
                    var delta = run[key] - lag;
                    run[key + '_label'] = run[key] + series.unitsShort + ' (' + (series.precision > 0 ? delta.toPrecision(series.precision) : delta) + series.unitsShort + ')';
                    run[key + '_class'] = delta > 0 ? (series.higherBetter ? 'app-good' : 'app-bad') : (series.higherBetter ? 'app-bad' : 'app-good');
                }
                lag = run[key];
            });
        });
        page.body.html(t.test.render({
            runs : runs
        }));
        var renderCharts = function() {
            v.each(series, function(series) {
                var key = series.key;
                var chart = w.nv.models.lineChart();
                chart.tooltipContent(function(_, x, y, e, graph) {
                    return '<h3>' + runs[x].name + '</h3>' + '<p>' + runs[x][key] + ' ' + series.units + '</p>';
                });
                chart.yAxis.axisLabel(series.name + ' (' + series.unitsShort + ')');
                chart.showLegend(false);
                var values = [];
                v.each(runs, function(run, i) {
                    if(run[key] !== null) {
                        values.push({
                            x : i,
                            y : run[key]
                        });
                    }
                });
                bonzo(qwery('#g-' + key)).empty();
                var sel = w.d3.select('#g-' + key).datum([
                    {
                        values : values
                    }
                ]);
                var obj = chart(sel);
                var point = null;
                sel.on('click', function() {
                    if(point !== null && point !== 0) {
                        page.go('/compare/' + moduleName + '/' + testName + '/' + runs[point - 1].name + '/' + runs[point].name);
                    }
                });
                sel.selectAll('.x').selectAll('.axis').remove();
                var tooltipShow = obj.dispatch.on('tooltipShow');
                var tooltipHide = obj.dispatch.on('tooltipHide');
                obj.dispatch.on('tooltipShow', function(e) {
                    point = e.pointIndex;
                    tooltipShow(e);
                });
                obj.dispatch.on('tooltipHide', function(e) {
                    point = null;
                    tooltipHide(e);
                });
            });
        };
        renderCharts();
        w.nv.utils.windowResize(renderCharts);
    });
    cb(null);
};

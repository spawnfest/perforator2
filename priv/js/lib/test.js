var v = require('valentine');
var qwery = require('qwery');
var bonzo = require('bonzo');
var bean = require('bean');
var t = require('./templates');
var w = require('./window');
var series = require('./series');
series = series.series;

exports.init = function(page, cb) {
    page.handle(/^\/(.+)\/test\/(.+)\/(.+)$/, function(from, to, params) {
        var state = {
            projectId : page.projectId,
            moduleName : params[0],
            testName : params[1]
        };
        page.req('test_runs', state, function(_, runs) {
            v.each(series, function(series) {
                var lag = null;
                var key = series.key;
                v.each(runs, function(run) {
                    run[key] = run[key].mean;
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
                moduleName : state.moduleName,
                testName : state.testName,
                runs : runs
            }));
            var renderCharts = function() {
                v.each(series, function(series) {
                    var key = series.key;
                    var chart = w.nv.models.lineChart();
                    chart.tooltipContent(function(_, x, y, e, graph) {
                        return '<h3>' + runs[x].build_id + '</h3>' + '<p>' + runs[x][key] + ' ' + series.units + '</p>';
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
                        if(point !== null) {
                            page.go('/' + state.projectId + '/compare/' + (runs[point - 1] ? runs[point - 1].build_id : '') + '-' + runs[point].build_id + '/' + state.moduleName + '-' + state.moduleName + '/' + state.testName + '-' + state.testName);
                        }
                    });
                    sel.selectAll('.x').selectAll('.axis').remove();
                    var tooltipShow = obj.dispatch.on('tooltipShow');
                    var tooltipHide = obj.dispatch.on('tooltipHide');
                    obj.dispatch.on('tooltipShow', function(e) {
                        if(e.pointIndex === 0) {
                            point = 1;
                        } else {
                            point = e.pointIndex;
                        }
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
    });
    cb(null);
};

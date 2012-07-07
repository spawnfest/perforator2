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
    page.handle(/^\/run\/(.+)$/, function() {
        var modules = [
            {
                name : 'test_modulea',
                tests : [
                    {
                        name : 'bla_blabla',
                        series : {
                            memutil : {
                                previous : 100,
                                mean : 90,
                                current : 110,
                                max : 200,
                                min : 50
                            },
                            time : {
                                previous : 311,
                                current : 300
                            }
                        }
                    }, {
                        name : 'what_ever',
                        series : {
                            memutil : {
                                previous : 150,
                                mean : 90,
                                current : 130,
                                max : 200,
                                min : 50
                            },
                            time : {
                                previous : 250,
                                current : 330
                            }
                        }
                    }
                ]
            }, {
                name : 'test_moduleb',
                tests : [
                    {
                        name : 'foo_bar',
                        series : {
                            memutil : {
                                previous : 100,
                                mean : 120,
                                current : 130,
                                max : 250,
                                min : 100
                            },
                            time : {
                                previous : 211,
                                current : 300
                            }
                        }
                    }, {
                        name : 'what_now',
                        series : {
                            memutil : {
                                previous : 120,
                                mean : 140,
                                current : 130,
                                max : 190,
                                min : 70
                            },
                            time : {
                                previous : 350,
                                current : 330
                            }
                        }
                    }
                ]
            }
        ];
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
            run : {
                id : 0
            }
        }));
        var select = bonzo(qwery('select'));
        bean.add(select[0], 'change', function() {
            setSeries(select.val());
        });
        var chart = w.nv.models.bulletChart();
        v.each(modules, function(module) {
            v.each(module.tests, function(test) {
                bean.add(qwery('#' + test.id)[0].parentNode.parentNode, 'click', function() {
                    page.go('/test/' + module.name + '/' + test.name);
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
    cb();
};

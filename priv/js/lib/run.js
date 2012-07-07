var t = require('./templates');
var moment = require('moment');
var bean = require('bean');
var bonzo = require('bonzo');
var qwery = require('qwery');
var w = require('./window');
var v = require('valentine');

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
                                mean : 300,
                                current : 300,
                                max : 400,
                                min : 200
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
                                mean : 300,
                                current : 330,
                                max : 400,
                                min : 200
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
                                mean : 300,
                                current : 300,
                                max : 400,
                                min : 200
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
                                mean : 300,
                                current : 330,
                                max : 400,
                                min : 200
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
        var series = [
            {
                name : 'Time',
                key : 'time',
                units : 'milliseconds',
                higherBetter : false
            }, {
                name : 'Average Memory Utilization',
                key : 'memutil',
                units : 'bytes',
                higherBetter : false
            }, {
                name : 'Average CPU Load',
                key : 'load',
                units : 'TODO',
                higherBetter : false
            }, {
                name : 'Average CPU Utilization',
                key : 'cpuutil',
                units : 'TODO',
                higherBetter : false
            }
        ];
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
        var setSeries = function(key) {
            v.each(modules, function(module) {
                v.each(module.tests, function(test) {
                    bonzo(qwery('#' + test.id)).empty();
                    w.d3.select('#' + test.id).datum({
                        "title":test.name,
                        "subtitle":seriesMap[key].units,
                        "ranges":[test.series[key].min, test.series[key].mean, test.series[key].max],
                        "measures":[test.series[key].current],
                        "markers":[test.series[key].previous]
                    }).call(chart);
                    bonzo(qwery('#' + test.id + ' .measure')).css('fill', test.series[key].current > test.series[key].previous ? (seriesMap[key].higherBetter ? '#0f0' : '#f00') : (seriesMap[key].higherBetter ? '#f00' : '#0f0'));
                });
            });
        };
        setSeries('time');
    });
    cb();
};

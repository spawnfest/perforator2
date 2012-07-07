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
        page.body.html(t.run.render({
            modules : modules,
            series : [
                {
                    name : 'Time',
                    key : 'time'
                }, {
                    name : 'Average Memory Utilization',
                    key : 'memutil'
                }, {
                    name : 'Average CPU Load',
                    key : 'load'
                }, {
                    name : 'Average CPU Utilization',
                    key : 'cpuutil'
                }
            ],
            run : {
                id : 0
            }
        }));
        var select = bonzo(qwery('select'));
        bean.add(select[0], 'change', function() {
            console.log('TODO', 'change series', select.val());
        });
        var chart = w.nv.models.bulletChart();
        v.each(modules, function(module) {
            v.each(module.tests, function(test) {
                w.d3.select('#' + test.id).datum({
                    "title":test.name,
                    "subtitle":'milliseconds',
                    "ranges":[test.series.time.min, test.series.time.mean, test.series.time.max],
                    "measures":[test.series.time.current],
                    "markers":[test.series.time.previous]
                }).call(chart);
                bonzo(qwery('#' + test.id + ' .measure')).css('fill', test.series.time.current > test.series.time.previous ? '#f00' : '#0f0');
            });
        });
    });
    cb();
};

var t = require('./templates');
var v = require('valentine');
var qwery = require('qwery');
var bean = require('bean');
var step = require('step');
var bonzo = require('bonzo');
var moment = require('moment');
var series = require('./series');
series = series.series;

exports.init = function(page, cb) {
    page.handle(/^\/(.+)\/compare\/(.+)\/(.+)\/(.+)$/, function(from, to, params) {
        var state = {
            projectId : params[0],
            runIds : params[1].split('-'),
            moduleNames : params[2].split('-'),
            testNames : params[3].split('-')
        };
        var runs = [];
        v.each(state.runIds, function(_, i) {
            runs.push({
                projectId : state.projectId,
                runId : state.runIds[i],
                moduleName : state.moduleNames[i],
                testName : state.testNames[i]
            });
        });
        step(function() {
            var group = this.group();
            v.each(runs, function(run) {
                page.req('testRun', null, run, group());
            });
        }, function(_, runs) {
            var runA = runs[0].run;
            var runB = runs[1].run;
            var numbers = [];
            v.each(series, function(series) {
                numbers.push({
                    series : series.name,
                    A : runA[series.key],
                    B : runB[series.key],
                    delta : runB[series.key] - runA[series.key]
                });
            });
            page.body.html(t.compare.render({
                runA : runA,
                runB : runB,
                modules : [
                    {
                        name : 'moduleA'
                    }, {
                        name : 'moduleB'
                    }, {
                        name : 'moduleC'
                    }
                ],
                runs : [
                    {
                        name : '6972067'
                    }, {
                        name : '2346734'
                    }, {
                        name : '2736297'
                    }
                ],
                tests : [
                    {
                        name : 'testA'
                    }, {
                        name : 'testB'
                    }, {
                        name : 'testC'
                    }
                ],
                commits : [
                    {
                        id : 'a8a7d85c8c',
                        shortDescription : 'implementing awesomeness'
                    },
                    {
                        id : 'a8a7d85c8c',
                        shortDescription : 'implementing awesomeness'
                    }
                ],
                numbers : numbers
            }));
        });
    });
    cb();
};

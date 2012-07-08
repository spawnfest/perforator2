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
            projectId : page.projectId,
            runIds : params[0].split('-'),
            moduleNames : params[1].split('-'),
            testNames : params[2].split('-')
        };
        var runs = [];
        v.each(state.runIds, function(_, i) {
            runs.push({
                projectId : parseInt(state.projectId, 10),
                runId : parseInt(state.runIds[i], 10),
                moduleName : state.moduleNames[i],
                testName : state.testNames[i]
            });
        });
        step(function() {
            var group = this.group();
            page.req('builds', state.projectId, group());
            v.each(runs, function(run) {
                var runCb = group();
                var modulesCb = group();
                var testsCb = group();
                page.req('build', run.runId, function(_, modules) {
                    modulesCb(null, modules);

                    var tests = null;
                    v.each(modules, function(module) {
                        if(module.name === run.moduleName) {
                            tests = module.test_cases;
                            v.each(tests, function(test) {
                                if(test.name === run.testName) {
                                    v.each(test.result, function(key, value) {
                                        test.result[key] = value.mean;
                                    });
                                    test.result.build_id = run.runId;
                                    runCb(null, test.result);
                                }
                            });
                        }
                    });
                    testsCb(null, tests);
                });
            });
        }, function(_, group) {
            console.log('compare', group);
            var runA = group[1];
            var modulesA = group[2];
            var testsA = group[3];

            var runB = group[4];
            var modulesB = group[5];
            var testsB = group[6];

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
                modulesA : modulesA,
                modulesB : modulesB,
                runs : group[0],
                testsA : testsA,
                testsB : testsB,
                commits : [],// TODO fill with real data
                numbers : numbers
            }));
            bonzo(qwery('#runA')).val(String(runs[0].runId));
            bonzo(qwery('#runB')).val(String(runs[1].runId));
            bonzo(qwery('#moduleA')).val(runs[0].moduleName);
            bonzo(qwery('#moduleB')).val(runs[1].moduleName);
            bonzo(qwery('#testA')).val(runs[0].testName);
            bonzo(qwery('#testB')).val(runs[1].testName);
            var change = function() {
                var runA = bonzo(qwery('#runA')).val();
                var runB = bonzo(qwery('#runB')).val();
                var moduleA = bonzo(qwery('#moduleA')).val();
                var moduleB = bonzo(qwery('#moduleB')).val();
                var testA = bonzo(qwery('#testA')).val();
                var testB = bonzo(qwery('#testB')).val();
                page.go('/' + state.projectId + '/compare/' + runA + '-' + runB + '/' + moduleA + '-' + moduleB + '/' + testA + '-' + testB);
            };
            v.each(qwery('select'), function(select) {
                bean.add(select, 'change', change);
            });
        });
    });
    cb();
};

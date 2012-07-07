var t = require('./templates');
var v = require('valentine');
var qwery = require('qwery');
var bean = require('bean');
var bonzo = require('bonzo');
var moment = require('moment');
var series = require('./series');
series = series.series;

exports.init = function(page, cb) {
    page.handle(/^\/compare\/(.+)\/(.+)\/(.+)\/(.+)$/, function(from, to, params) {
        var moduleName = params[0];
        var testName = params[1];
        var runA = {
            name : params[2],
            time : 100,
            load : 1.0,
            memutil : 30000,
            cpuutil : 0.9
        };
        var runB = {
            name : params[3],
            time : 100,
            load : 1.0,
            memutil : 30000,
            cpuutil : 0.9
        };
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
    cb();
};

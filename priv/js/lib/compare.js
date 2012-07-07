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
        v.each(qwery('tr'), function(row) {
            bean.add(row, 'click', function(e) {
                console.log(bonzo(row).data('id'));
                page.go('/run/' + bonzo(row).data('id'));
            });
        });
    });
    cb();
};

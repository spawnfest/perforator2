var t = require('./templates');
var moment = require('moment');
var w = require('./window');
var v = require('valentine');

exports.init = function(page, cb) {
    page.handle(/^\/run\/(.+)$/, function() {
        var tests = [
            {
                name : 'bla_blabla',
                id : 'test-bla_blabla',
                time : '0.3s, -0.01s',
                style : 'app-good'
            }, {
                name : 'what_ever',
                id : 'test-what_ever',
                time : '0.2s, +0.03s',
                style : 'app-bad'
            }
        ];
        page.body.html(t.run.render({
            tests : tests,
            run : {
                id : 0
            }
        }));
        var chart = w.nv.models.bulletChart();
        v.each(tests, function(test) {
            w.d3.select('#' + test.id).datum({
                "title":test.name,
                "subtitle":'milliseconds',
                "ranges":[150,225,300],
                "measures":[220],
                "markers":[250]
            }).call(chart);
        });
    });
    cb();
};

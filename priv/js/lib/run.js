var t = require('./templates');
var moment = require('moment');
var bonzo = require('bonzo');
var qwery = require('qwery');
var w = require('./window');
var v = require('valentine');

exports.init = function(page, cb) {
    page.handle(/^\/run\/(.+)$/, function() {
        var tests = [
            {
                name : 'bla_blabla',
                time : {
                    previous : 311,
                    mean : 300,
                    current : 300,
                    max : 400,
                    min : 200
                }
            }, {
                name : 'what_ever',
                time : {
                    previous : 250,
                    mean : 300,
                    current : 330,
                    max : 400,
                    min : 200
                }
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
            var id = 'test-' + test.name;
            w.d3.select('#' + id).datum({
                "title":test.name,
                "subtitle":'milliseconds',
                "ranges":[test.time.min, test.time.mean, test.time.max],
                "measures":[test.time.current],
                "markers":[test.time.previous]
            }).call(chart);
            bonzo(qwery('#' + id + ' .measure')).css('fill', test.time.current > test.time.previous ? '#f00' : '#0f0');
        });
    });
    cb();
};

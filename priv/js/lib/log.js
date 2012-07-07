var t = require('./templates');
var v = require('valentine');
var qwery = require('qwery');
var bean = require('bean');
var bonzo = require('bonzo');
var moment = require('moment');

exports.init = function(page, cb) {
    page.handle('/', function() {
        page.go('/project/3ttat');
    });
    page.handle(/^\/project\/(.+)$/, function(from, to, params) {
        var id = params[0];
        page.body.html(t.log.render({
            currentId : id,
            projects : [
                {
                    id : '3ttat',
                    title : 'Project omg #1',
                    opened : id === '3ttat'
                },
                {
                    id : '3hsdg',
                    title : 'Project omg #2',
                    opened : id === '3hsdg'
                }
            ],
            runs : [
                {
                    id : 0,
                    started : moment(new Date()).fromNow(),
                    duration : 1000,
                    durationDelta : 10,
                    modules : 2,
                    tests : 4
                }, {
                    id : 1,
                    started : moment(new Date()).fromNow(),
                    duration : 900,
                    durationDelta : -100,
                    modules : 2,
                    tests : 4
                }
            ],
            project : {
                title : 'Project omg #1'
            }
        }));
        v.each(qwery('tr'), function(row) {
            bean.add(row, 'click', function() {
                page.go('/run/' + bonzo(row).data('id'));
            });
        });
    });
    cb();
};

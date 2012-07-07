var t = require('./templates');
var v = require('valentine');
var qwery = require('qwery');
var bean = require('bean');
var bonzo = require('bonzo');
var moment = require('moment');

exports.init = function(page, cb) {
    page.handle('/', function() {
        page.body.html(t.log.render({
            projects : [
                {
                    id : '3ttat',
                    title : 'Project omg #1',
                    opened : true
                },
                {
                    id : '3hsdg',
                    title : 'Project omg #2',
                    opened : false
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
        v.each(qwery('.app-edit'), function(edit) {
            bean.add(edit, 'click', function() {
                page.go('/project/' + bonzo(edit).data('id') + '/edit');
            });
        });
        v.each(qwery('tr'), function(row) {
            bean.add(row, 'click', function() {
                page.go('/run/' + bonzo(row).data('id'));
            });
        });
    });
    cb();
};

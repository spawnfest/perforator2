var t = require('./templates');
var moment = require('moment');

exports.init = function(page, cb) {
    page.handle('/', function() {
        page.body.html(t.log.render({
            projects : [
                {
                    title : 'Project omg #1',
                    opened : true
                },
                {
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
    });
    cb();
};

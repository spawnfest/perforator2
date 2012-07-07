var t = require('./templates');

exports.init = function(page, cb) {
    page.handle('/', function() {
        page.body.html(t.test.render({
            name : 'Vytautas'
        }));
    });
    cb();
};

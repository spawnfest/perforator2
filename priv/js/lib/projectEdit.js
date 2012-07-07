var t = require('./templates');
var v = require('valentine');
var qwery = require('qwery');
var bean = require('bean');
var bonzo = require('bonzo');
var moment = require('moment');

exports.init = function(page, cb) {
    page.handle(/^\/project\/(.+)\/edit$/, function() {
        page.body.html(t.projectEdit.render({
        }));
    });
    cb();
};

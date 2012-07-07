var t = require('./templates');
var v = require('valentine');
var qwery = require('qwery');
var bean = require('bean');
var bonzo = require('bonzo');
var moment = require('moment');

exports.init = function(page, cb) {
    page.handle(/^\/project\/add$/, function() {
        page.body.html(t.projectEdit.render({
            action : 'Add'
        }));
        bean.add(qwery('form')[0], 'submit', function(e) {
            page.emit('addProject', null, {
                title : bonzo(qwery('#title')).val(),
                repo : {
                    url : bonzo(qwery('#url')).val(),
                    type : 'git'
                }
            });
            page.once('projectAdded', function(_, project) {
                page.go('/project/' + project.id);
            });
            e.preventDefault();
        });
    });
    page.handle(/^\/project\/(.+)\/edit$/, function() {
        page.body.html(t.projectEdit.render({
            action : 'Save'
        }));
        bean.add(qwery('form')[0], 'submit', function(e) {
            e.preventDefault();
        });
    });
    cb();
};

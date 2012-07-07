var t = require('./templates');
var v = require('valentine');
var qwery = require('qwery');
var bean = require('bean');
var bonzo = require('bonzo');
var moment = require('moment');

exports.init = function(page, cb) {
    page.handle(/^\/add$/, function() {
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
                page.go('/' + project.id);
            });
            e.preventDefault();
        });
    });
    page.handle(/^\/(.+)\/edit$/, function(from, to, params) {
        page.req('project', null, params[0], function(_, project) {
            page.body.html(t.projectEdit.render({
                project : project,
                action : 'Save'
            }));
            bean.add(qwery('form')[0], 'submit', function(e) {
                page.emit('updateProject', null, {
                    id : project.id,
                    title : bonzo(qwery('#title')).val(),
                    repo : {
                        url : bonzo(qwery('#url')).val(),
                        type : 'git'
                    }
                });
                page.go('/' + project.id);
                e.preventDefault();
            });
        });
    });
    cb();
};

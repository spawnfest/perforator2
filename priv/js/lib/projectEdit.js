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
            var project = {
                name : bonzo(qwery('#name')).val(),
                repo_url : bonzo(qwery('#repo_url')).val(),
                branch : bonzo(qwery('#branch')).val(),
                build_instructions : [],
                polling_strategy : 'ondemand'
            };
            page.req('project/new', project, function(_, id) {
                project.id = id;
                bean.fire(page, 'projectAdded', [project]);
                page.go('/' + project.id);
            });
            e.preventDefault();
        });
    });
    page.handle(/^\/(.+)\/edit$/, function(from, to, params) {
        page.req('project', page.projectId, function(_, project) {
            page.body.html(t.projectEdit.render({
                project : project,
                action : 'Save'
            }));
            bean.add(qwery('form')[0], 'submit', function(e) {
                project = {
                    id : project.id,
                    name : bonzo(qwery('#name')).val(),
                    repo_url : bonzo(qwery('#repo_url')).val(),
                    branch : bonzo(qwery('#branch')).val(),
                    build_instructions : [],
                    polling_strategy : 'ondemand'
                };
                page.req('project/update', project);
                bean.fire(page, 'projectUpdated', [project]);
                page.go('/' + project.id);
                e.preventDefault();
            });
        });
    });
    cb();
};

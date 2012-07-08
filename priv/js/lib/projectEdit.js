var t = require('./templates');
var v = require('valentine');
var qwery = require('qwery');
var bean = require('bean');
var bonzo = require('bonzo');
var moment = require('moment');

exports.init = function(page, cb) {
    var gather = function() {
        var ondemand = bonzo(qwery('#ondemand')).attr('checked');
        return {
            name : bonzo(qwery('#name')).val(),
            repo_url : bonzo(qwery('#repo_url')).val(),
            branch : bonzo(qwery('#branch')).val(),
            build_instructions : ['one', 'two'],
            polling_strategy : (ondemand ? 'ondemand' : {
                time : parseInt(bonzo(qwery('#time')).val(), 10)
            })
        };
    };
    var augment = function() {
        bean.add(qwery('#ondemand')[0], 'click', function() {
            if(gather().ondemand) {
                bonzo(qwery('#time')).attr('disabled', 'disabled');
            } else {
                bonzo(qwery('#time')).removeAttr('disabled');
            }
        });
    };
    var adapt = function(project) {
        var ondemand = project.polling_strategy === 'ondemand';
        return {
            name : project.name,
            repo_url : project.repo_url,
            branch : project.branch,
            build_instructions : v.map(project.build_instructions, function(instruction) {
                return {
                    instruction : instruction
                };
            }),
            ondemand : ondemand,
            polling_strategy : project.polling_strategy
        };
    };
    page.handle(/^\/add$/, function() {
        page.body.html(t.projectEdit.render({
            project : adapt({
                name : 'TEST',
                repo_url : 'file:///home/tahu/test/repo',
                branch : 'origin/master',
                build_instructions : [ 'one', 'two' ],
                polling_strategy : {
                    time : 10000
                }
            }),
            action : 'Add project'
        }, t));
        augment();
        bean.add(qwery('form')[0], 'submit', function(e) {
            var project = gather();
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
                project : adapt(project),
                action : 'Save project'
            }, t));
            augment();
            bean.add(qwery('form')[0], 'submit', function(e) {
            var ondemand = bonzo(qwery('#ondemand')).attr('checked');
                project = gather();
                page.req('project/update', project);
                bean.fire(page, 'projectUpdated', [project]);
                page.go('/' + project.id);
                e.preventDefault();
            });
        });
    });
    cb();
};

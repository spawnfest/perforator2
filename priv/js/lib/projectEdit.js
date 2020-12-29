var t = require('./templates');
var w = require('./window');
var v = require('valentine');
var qwery = require('qwery');
var bean = require('bean');
var bonzo = require('bonzo');

exports.init = function(page, cb) {
    var gather = function() {
        var ondemand = w.el('ondemand').attr('checked');
        return {
            name : bonzo(qwery('#name')).val(),
            repo_url : bonzo(qwery('#repo_url')).val(),
            branch : bonzo(qwery('#branch')).val(),
            build_instructions : v.map(qwery('.app-build_instruction'), function(bi) {
                return bonzo(bi).val();
            }),
            polling_strategy : (ondemand ? 'ondemand' : {
                time : parseInt(bonzo(qwery('#time')).val(), 10)
            })
        };
    };
    var augment = function() {
        var add = qwery('.app-add')[0];
        bean.add(add, 'click', function() {
            var removes = qwery('.app-remove');
            var r = bonzo(add).before(t.build_instruction.render({
                instruction : ''
            }));
            augment.bindRemove(qwery('.app-remove', r.previous())[0]);
        });
        v.each(qwery('.app-remove'), function(remove) {
            augment.bindRemove(remove);
        });
        bean.add(w.el('ondemand')[0], 'click', function() {
            console.log('click', gather());
            if(gather().polling_strategy === 'ondemand') {
                w.el('time').attr('disabled', 'disabled');
            } else {
                w.el('time').removeAttr('disabled');
            }
        });
    };
    augment.bindRemove = function(remove) {
        bean.add(remove, 'click', function() {
            bonzo(remove.parentNode).remove();
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
                name : 'Perforator',
                repo_url : 'file:///home/tahu/Desktop/PERFORATOR/perforator',
                branch : 'origin/master',
                build_instructions : [ './rebar get-deps', './rebar compile', './rebar perf' ],
                polling_strategy : 'ondemand'
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
                var p = gather();
                p.id = project.id;
                page.req('project/update', p);
                bean.fire(page, 'projectUpdated', [p]);
                page.go('/' + p.id);
                e.preventDefault();
            });
        });
    });
    cb();
};

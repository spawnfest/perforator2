exports.init = function(page, cb) {
    page.on('addProject', function(_, project) {
        project.id = String(Math.random());
        page.emit('projectAdded', null, project);
    });
    page.on('req_runs', function(_, projectId) {
        page.emit('res_runs', null, {
            projectId : projectId,
            runs : [
                {
                    id : '8888',
                    started : new Date().getTime(),
                    time : 1000,
                    modules : 2,
                    tests : 4
                }, {
                    id : '8808',
                    started : new Date().getTime(),
                    time : 900,
                    modules : 2,
                    tests : 4
                }
            ]
        });
    });
    page.on('req_projects', function(_, m) {
        page.emit('res_projects', null, [ {
                id : '3ttat',
                title : 'Project omg #1',
                repo : {
                    type : 'git',
                    url : 'git@github.com:omg/proj1'
                }
            }, {
                id : '3hsdg',
                title : 'Project omg #2000',
                repo : {
                    type : 'git',
                    url : 'git@github.com:omg/proj2'
                }
            }
        ]);
    });
    cb(null);
};

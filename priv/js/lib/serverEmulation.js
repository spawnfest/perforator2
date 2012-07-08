exports.post = function(page, req, cb) {
    if(page === 'tests') {
        cb(null, [
            {
                name : 'testA'
            }, {
                name : 'testB'
            }, {
                name : 'testC'
            }
        ]);
    } else if(page === 'modules') {
        cb(null, [
            {
                name : 'moduleA'
            }, {
                name : 'moduleB'
            }, {
                name : 'moduleC'
            }
        ]);
    } else if(page === 'test_run') {
        cb(null, {
            name : req.runId,
            duration : 92000,
            cpu_load : 1,
            used_memory : 122000000,
            cpu_util : 0.9
        });
    } else if(page === 'test_runs') {
        cb(null, [
            {
                name : '8888',
                duration : 92000,
                cpu_load : 1,
                used_memory : 122000000,
                cpu_util : 0.9
            }, {
                name : '8008',
                duration : 72000,
                cpu_load : 0.7,
                used_memory : 922000000,
                cpu_util : 0.4
            }, {
                name : '8808',
                duration : 78000,
                cpu_load : 1.2,
                used_memory : 102000000,
                cpu_util : 1.0
            }
        ]);
    } else if(page === 'build') {
        cb(null, [{
            name : 'moduleA',
            tests : [
                {
                    name : 'testA',
                    successful : true,
                    result : {
                        used_memory : {
                            mean : 90,
                            max : 200,
                            min : 50
                        },
                        duration : {
                            mean : 90,
                            max : 200,
                            min : 50
                        }
                    }
                }, {
                    name : 'testB',
                    successful : true,
                    result : {
                        used_memory : {
                            mean : 90,
                            max : 200,
                            min : 50
                        },
                        duration : {
                            mean : 90,
                            max : 200,
                            min : 50
                        }
                    }
                }
            ]
        }, {
            name : 'moduleB',
            tests : [
                {
                    name : 'testA',
                    successful : false,
                    result : null
                }, {
                    name : 'testB',
                    successful : true,
                    result : {
                        used_memory : {
                            mean : 140,
                            max : 190,
                            min : 70
                        },
                        duration : {
                            mean : 140,
                            max : 190,
                            min : 70
                        }
                    }
                }
            ]
        }]);
    } else if(page === 'project/update') {
        cb(null, null);
    } else if(page === 'project/new') {
        cb(null, Math.floor(Math.random() * 1000000));
    } else if(page === 'builds') {
        cb(null, [
            {
                id : '8888',
                started : Math.floor(new Date().getTime() / 1000),
                commit_id : '23523623',
                succeeded : true,
                time : 1000,
                modules : 2,
                tests : 4
            }, {
                id : '8008',
                started : Math.floor(new Date().getTime() / 1000),
                commit_id : '23523623',
                succeeded : false,
                time : 800,
                modules : 2,
                tests : 4
            }, {
                id : '8808',
                started : Math.floor(new Date().getTime() / 1000),
                commit_id : '23523623',
                succeeded : true,
                time : 900,
                modules : 2,
                tests : 4
            }
        ]);
    } else if(page === 'project') {
        cb(null, {
            id : req,
            name : 'Project omg #' + req,
            repo_url : 'git://github.com/linus/linux',
            branch : 'master',
            build_instructions : [],
            polling_strategy : 'ondemand'
        });
    } else if(page === 'builders') {
        cb(null, [{
            name : 'Wo00t',
            queue_size : 10
        }, {
            name : 'WorkWork',
            queue_size : 0
        }]);
    } else if(page === 'previous_build') {
        cb(null, req);
    } else if(page === 'projects') {
        cb(null, [ {
            id : 1,
            name : 'Project omg #1',
            repo_url : 'git://github.com/linus/linux',
            branch : 'master',
            build_instructions : [],
            polling_strategy : 'ondemand'
        }, {
            id : 2000,
            name : 'Project omg #2000',
            repo_url : 'git://github.com/linus/linux',
            branch : 'master',
            build_instructions : [],
            polling_strategy : 'ondemand'
        } ]);
    }
};

exports.init = function(page, cb) {
    cb(null);
};

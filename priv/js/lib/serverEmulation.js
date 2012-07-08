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
    } else if(page === 'testRun') {
        cb(null, {
            name : req.runId,
            time : 92000,
            load : 1,
            memutil : 122000000,
            cpuutil : 0.9
        });
    } else if(page === 'testRuns') {
        cb(null, [
            {
                name : '8888',
                time : 92000,
                load : 1,
                memutil : 122000000,
                cpuutil : 0.9
            }, {
                name : '8008',
                time : 72000,
                load : 0.7,
                memutil : 922000000,
                cpuutil : 0.4
            }, {
                name : '8808',
                time : 78000,
                load : 1.2,
                memutil : 102000000,
                cpuutil : 1.0
            }
        ]);
    } else if(page === 'run') {
        cb(null, [{
            name : 'moduleA',
            tests : [
                {
                    name : 'testA',
                    series : {
                        memutil : {
                            previous : 100,
                            mean : 90,
                            current : 110,
                            max : 200,
                            min : 50
                        },
                        time : {
                            previous : 311,
                            current : 300
                        }
                    }
                }, {
                    name : 'testB',
                    series : {
                        memutil : {
                            previous : 150,
                            mean : 90,
                            current : 130,
                            max : 200,
                            min : 50
                        },
                        time : {
                            previous : 250,
                            current : 330
                        }
                    }
                }
            ]
        }, {
            name : 'moduleB',
            tests : [
                {
                    name : 'testA',
                    series : {
                        memutil : {
                            previous : 100,
                            mean : 120,
                            current : 130,
                            max : 250,
                            min : 100
                        },
                        time : {
                            previous : 211,
                            current : 300
                        }
                    }
                }, {
                    name : 'testB',
                    series : {
                        memutil : {
                            previous : 120,
                            mean : 140,
                            current : 130,
                            max : 190,
                            min : 70
                        },
                        time : {
                            previous : 350,
                            current : 330
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
    /* TODO
    setTimeout(function() {
        page.emit('queue_size', null, {
            name : 'perforator_ci@laptop',
            queue_size : 10
        });
        }, 2000);
    */
    cb(null);
};

var hogan = require('hogan.js');
var browserify = require('browserify');
var v = require('valentine');
var fs = require('fs');
var dir = __dirname + '/../lib';

var compile = function(type) {
    if(type !== 'change') {
        return;
    }
    console.log('recompiling', arguments);
    var b = browserify({
        debug : true
    });
    b.addEntry('./lib/index.js');
    fs.writeFileSync(__dirname + '/../../www/browserify.js', b.bundle());
};

compile();
if(process.argv[2] === 'watch') {
    fs.watch(dir, compile);
}


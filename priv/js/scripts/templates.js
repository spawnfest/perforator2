var hogan = require('hogan.js');
var v = require('valentine');
var fs = require('fs');
var dir = __dirname + '/../views';

var compile = function() {
    console.log('recompiling');
    var templates = 'var hogan = require(\'hogan.js\');';
    v.each(fs.readdirSync(dir), function(file) {
        var text = fs.readFileSync(dir + '/' + file).toString('utf8');
        templates += 'exports.' + file.substr(0, file.lastIndexOf('.')) +
            ' = new hogan.Template(' + hogan.compile(text, {
            asString : 1
        }) + ');';
    });
    fs.writeFileSync(__dirname + '/../lib/templates.js', templates);
};

compile();
if(process.argv[2] === 'watch') {
    fs.watch(dir, compile);
}


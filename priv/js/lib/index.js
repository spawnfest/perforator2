var t = require('./templates');
var qwery = require('qwery');
var domready = require('domready');
var bonzo = require('bonzo');

domready(function() {
    bonzo(qwery('#body')).html(t.test.render({
        name : 'Vytautas'
    }));
});

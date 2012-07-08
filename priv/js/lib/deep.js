var equal = require('deep-equal');
var bean = require('bean');
var hasOwn = Object.prototype.hasOwnProperty;

var copy = function(source, target) {
    var i, j, found;
    for(i in source) {
        if(hasOwn(source, i)) {
            if(hasOwn(target, i)) {
                if(['number', 'string', 'boolean'].indexOf(typeof source[i]) < 0) {
                    if(source[i] === null) {
                        target[i] = null;
                    } else {
                        copy(source[i], target[i]);
                    }
                } else {
                    target[i] = source[i];
                }
            } else {
                target[i] = source[i];
            }
        }
    }
    for(i in target) {
        if(hasOwn(target, i) && !hasOwn(source, i)) {
            delete target[i];
        }
    }
};

exports.update = function(target, source) {
    var deleted = [];
    var inserted = [];
    var updated = [];
    var i, j, found;
    for(i = 0; i < source.length; i += 1) {
        found = false;
        for(j = 0; j < target.length; j += 1) {
            if(source[i].id === target[j].id) {
                found = true;
                if(!equal(source[i], target[j])) {
                    copy(source[i], target[j]);
                    bean.fire(target[j], 'update');
                    updated.push(target[i]);
                }
                break;
            }
        }
        if(!found) {
            inserted.push(source[i]);
        }
    }
    for(i = 0; i < target.length; i += 1) {
        found = false;
        for(j = 0; j < source.length; j += 1) {
            if(source[j].id === target[i].id) {
                found = true;
                break;
            }
        }
        if(!found) {
            deleted.push(target[i]);
            bean.fire(target[i], 'delete');
            target.splice(i, 1);
            i -= 1;
        }
    }
    for(i = 0; i < inserted.length; i += 1) {
        target.push(inserted[i]);
    }
    return {
        updated : updated,
        inserted : inserted,
        deleted : deleted
    };
};


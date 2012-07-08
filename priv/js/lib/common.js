exports.findById = function(arr, id) {
    return exports.findBy(arr, 'id', id);
};
exports.findBy = function(arr, key, val) {
    for(var i = 0; i < arr.length; i += 1) {
        if(arr[i][key] === val) {
            return arr[i];
        }
    }
    return null;
};

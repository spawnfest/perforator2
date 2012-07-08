exports.findById = function(arr, id) {
    for(var i = 0; i < arr.length; i += 1) {
        if(arr[i].id === id) {
            return arr[i];
        }
    }
    return null;
};

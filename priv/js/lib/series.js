var series = [
    {
        name : 'Time',
        key : 'duration',
        units : 'milliseconds',
        unitsShort : 'ms',
        precision : 0,
        higherBetter : false,
        adapt : function(val) {
            return {
                val : val,
                unitsShort : 'ms',
                units : 'milliseconds'
            };
        }
    }, {
        name : 'Average Memory Utilization',
        key : 'used_memory',
        units : 'bytes',
        unitsShort : 'b',
        precision : 0,
        higherBetter : false,
        adapt : function(val) {
            return {
                val : val,
                unitsShort : 'b',
                units : 'bytes'
            };
        }
    }, {
        name : 'Average CPU Load',
        key : 'cpu_load',
        units : '',
        unitsShort : '',
        precision : 2,
        higherBetter : false,
        adapt : function(val) {
            return {
                val : val,
                unitsShort : '',
                units : ''
            };
        }
    }, {
        name : 'Average CPU Utilization',
        key : 'cpu_util',
        units : 'percent',
        unitsShort : '%',
        precision : 2,
        higherBetter : false,
        adapt : function(val) {
            return {
                val : val,
                unitsShort : '%',
                units : 'percent'
            };
        }
    }
];
exports.series = series;

var series = [
    {
        name : 'Time',
        key : 'duration',
        units : 'milliseconds',
        unitsShort : 'ms',
        precision : 0,
        higherBetter : false
    }, {
        name : 'Average Memory Utilization',
        key : 'used_memory',
        units : 'bytes',
        unitsShort : 'b',
        precision : 0,
        higherBetter : false
    }, {
        name : 'Average CPU Load',
        key : 'cpu_load',
        units : '',
        unitsShort : '',
        precision : 2,
        higherBetter : false
    }, {
        name : 'Average CPU Utilization',
        key : 'cpu_util',
        units : 'percent',
        unitsShort : '%',
        precision : 2,
        higherBetter : false
    }
];
exports.series = series;

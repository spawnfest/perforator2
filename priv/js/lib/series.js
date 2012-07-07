var series = [
    {
        name : 'Time',
        key : 'time',
        units : 'milliseconds',
        unitsShort : 'ms',
        precision : 0,
        higherBetter : false
    }, {
        name : 'Average Memory Utilization',
        key : 'memutil',
        units : 'bytes',
        unitsShort : 'b',
        precision : 0,
        higherBetter : false
    }, {
        name : 'Average CPU Load',
        key : 'load',
        units : 'TODO',
        unitsShort : '?',
        precision : 2,
        higherBetter : false
    }, {
        name : 'Average CPU Utilization',
        key : 'cpuutil',
        units : 'TODO',
        unitsShort : '?',
        precision : 2,
        higherBetter : false
    }
];
exports.series = series;

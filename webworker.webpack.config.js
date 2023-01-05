const path = require('path');

module.exports = {
    entry: './src/metamath/worker/webworker-main.js',
    output: {
        filename: 'webworker-main.js',
        path: path.resolve(__dirname, 'dist'),
    },
    mode: 'production'
};
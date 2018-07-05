const webpack = require("webpack");

module.exports = {
    entry: './js/app.js',
    output: {
        path: __dirname + "/target/js",
        filename: 'bundle.js'
    },
    plugins: [
      new webpack.ProvidePlugin({
          d3: 'd3'
      })
    ]
};

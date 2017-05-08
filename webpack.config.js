var path = require("path");

module.exports = {
    entry: {
        app: [
            './src/index.js'
        ]
    },

    output: {
        path: path.resolve(__dirname + '/public'),
        filename: '[name].js',
    },

    module: {
        rules: [
            {
                test: /\.(css|scss)$/,
                use: [
                    { loader: 'style-loader' },
                    { loader: 'css-loader' },
                ],
            },
            {
                test:    /\.html$/,
                exclude: /node_modules/,
                loader:  'file-loader',
                options: {
                    name: '[name].[ext]',
                },
            },
            {
                test:    /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                loader:  'elm-webpack-loader',
                options: {
                    verbose: true,
                    warn: true,
                },
            },
            {
                test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
                loader: 'url-loader',
                options: {
                    limit: 10000,
                    mimetype: 'application/font-woff',
                },
            },
            {
                test: /\.(ttf|eot|svg)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
                loader: 'file-loader',
            },
        ],

        noParse: /\.elm$/,
    },

    devServer: {
        historyApiFallback: true,
        inline: true,
        stats: { colors: true },
    },

};

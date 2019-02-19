// shared config (dev and prod)
const {resolve} = require('path');
const {CheckerPlugin} = require('awesome-typescript-loader');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const ManifestPlugin = require('webpack-manifest-plugin');
const CopyPlugin = require('copy-webpack-plugin');

module.exports = {
  resolve: {
    extensions: ['.ts', '.tsx', '.js', '.jsx'],
  },
  context: resolve(__dirname, '../../src'),
  module: {
    rules: [
      {
        test: /\.js$/,
        use: ['babel-loader'], //, 'source-map-loader'
        exclude: /node_modules/,
      },
      {
        test: /\.tsx?$/,
        use: ['babel-loader', 'awesome-typescript-loader'],
      },
      {
        test: /\.css$/,
        use: ['style-loader', { loader: 'css-loader', options: { importLoaders: 1 } }],
      },
      {
        test: /\.scss$/,
        loaders: [
          'style-loader',
          { loader: 'css-loader', options: { importLoaders: 1 } },
          'sass-loader',
        ],
      },
      {
        test: /\.(jpe?g|png|gif|svg)$/i,
        loaders: [
          'file-loader?hash=sha512&digest=hex&name=img/[hash].[ext]',
          'image-webpack-loader?bypassOnDebug&optipng.optimizationLevel=7&gifsicle.interlaced=false',
        ],
      },
    ],
  },
  plugins: [
    new CheckerPlugin(),
    new HtmlWebpackPlugin({template: 'index.html',}),
    new ManifestPlugin({
      fileName: 'asset-manifest.json',
      generate: (seed, files) => files.reduce((manifest, {name, path}) => ({
        ...manifest,
        [name]: path
      }), seed)
    }),
    new ManifestPlugin({
      fileName: 'manifest.json',
      generate: (seed, files) => files.reduce((manifest) => ({
        ...manifest, 
        "short_name": "NITTA Web UI",
        "name": "NITTA Web UI",
        "icons": [
          {
            "src": "favicon.ico",
            "sizes": "64x64 32x32 24x24 16x16",
            "type": "image/x-icon"
          }
        ],
        "start_url": "./index.html",
        "display": "standalone",
        "theme_color": "#000000",
        "background_color": "#ffffff"
      }), seed)
    }),
    new CopyPlugin([
      { from: './assets/img/favicon.ico' },
    ]),
  ],
  externals: {
    // 'react': 'React',
    // 'react-dom': 'ReactDOM',
  },
  performance: {
    hints: false,
  },
};

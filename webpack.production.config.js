const webpack = require("webpack");
const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const CleanWebpackPlugin = require("clean-webpack-plugin");
const TsconfigPathsPlugin = require('tsconfig-paths-webpack-plugin');
const BrowserSyncPlugin = require('browser-sync-webpack-plugin');

module.exports = {
  entry: path.join(__dirname, "src/app/main.ts"),
  output: {
    path: path.join(__dirname, "dist"),
    filename: "js/bundle.js"
  },
  resolve: {
    plugins: [
      new TsconfigPathsPlugin({
        configFile: path.join(__dirname, "tsconfig.json")
      })
    ],
    extensions: [".ts", ".js"],
    alias: {
      pixi: path.join(__dirname, "node_modules/phaser-ce/build/custom/pixi.js"),
      phaser: path.join(__dirname, "node_modules/phaser-ce/build/custom/phaser-split.js"),
      p2: path.join(__dirname, "node_modules/phaser-ce/build/custom/p2.js"),
      assets: path.join(__dirname, "assets/")
    }
  },
  plugins: [
    new CleanWebpackPlugin([
      path.join(__dirname, "dist")
    ]),
    new webpack.optimize.UglifyJsPlugin({
      drop_console: true,
      minimize: true,
      output: {
        comments: false
      }
    }),
    new HtmlWebpackPlugin({
      title: "TeaFlo",
      template: path.join(__dirname, "templates/index.ejs")
    }),
  ],
  module: {
    rules: [
      { test: /\.ts$/, enforce: "pre", loader: "tslint-loader" },
      { test: /assets(\/|\\)/, loader: "file-loader?name=assets/[hash].[ext]" },
      { test: /pixi\.js$/, loader: "expose-loader?PIXI" },
      { test: /phaser-split\.js$/, loader: "expose-loader?Phaser" },
      { test: /p2\.js$/, loader: "expose-loader?p2" },
      { test: /\.ts$/, loader: "ts-loader", exclude: "/node_modules/" }
    ]
  }
};

const webpack = require("webpack");
const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const CopyWebpackPlugin = require("copy-webpack-plugin");
const CleanWebpackPlugin = require("clean-webpack-plugin");
const BrowserSyncPlugin = require("browser-sync-webpack-plugin");
const BundleAnalyzerPlugin = require("webpack-bundle-analyzer").BundleAnalyzerPlugin;

module.exports = {
  entry: path.join(__dirname, "src/app/main.ts"),
  output: {
    path: path.join(__dirname, "dist"),
    filename: "js/bundle.js"
  },
  resolve: {
    plugins: [],
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
    new CopyWebpackPlugin([
      { from: "assets/", to: "assets/" },
    ]),
    new HtmlWebpackPlugin({
      title: "TeaFlo",
      template: path.join(__dirname, "templates/index.ejs")
    }),
    new BrowserSyncPlugin({
      host: process.env.IP || 'localhost',
      port: process.env.PORT || 3000,
      server: {
        baseDir: ['./', './dist']
      }
    })
    // , new BundleAnalyzerPlugin()
  ],
  module: {
    rules: [
      { test: /assets(\/|\\)/,
        loader: "file-loader?name=assets/[hash].[ext]",
      },
      { test: /pixi\.js$/,
        loader: "expose-loader?PIXI",
      },
      { test: /phaser-split\.js$/,
        loader: "expose-loader?Phaser",
      },
      { test: /p2\.js$/,
        loader: "expose-loader?p2",
      },
      { test: /\.ts$/,
        loader: "ts-loader",
        exclude: "/node_modules/",
      },
    ]
  },
  devtool: "source-map"
};

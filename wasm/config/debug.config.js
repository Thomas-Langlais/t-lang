const path = require("path");
const fs = require("fs");
const CopyPlugin = require("copy-webpack-plugin");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const WasmPackPlugin = require("@wasm-tool/wasm-pack-plugin");

const executingDir = fs.realpathSync(process.cwd());
const dist = path.resolve(executingDir, "dist");

module.exports = {
  entry: {
    index: "./js/index.js"
  },
  output: {
    path: dist,
  },
  devServer: {
    hot: true,
  },
  mode: "development",
  plugins: [
    new HtmlWebpackPlugin(),
    new WasmPackPlugin({
      crateDirectory: ".",
      outName: "tlang_wasm"
    })
  ],
  module: {
    rules: [
      {
        test: /\.(css)$/,
        use: ['style-loader', 'css-loader'],
      }
    ]
  },
  experiments: {
    asyncWebAssembly: true
  },
};

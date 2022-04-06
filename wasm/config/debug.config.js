const path = require("path");
const fs = require("fs");
const CopyPlugin = require("copy-webpack-plugin");

const executingDir = fs.realpathSync(process.cwd());
const dist = path.resolve(executingDir, "dist");

module.exports = {
  mode: "development",
  entry: {
    index: "./js/index.js"
  },
  output: {
    path: dist,
    filename: "[name].js"
  },
  devServer: {
    watchFiles: './js/*'
  },
  experiments: {
    asyncWebAssembly: true
  },
  plugins: [
    new CopyPlugin([
      path.resolve("static")
    ]),
  ]
};

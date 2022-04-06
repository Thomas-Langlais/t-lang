const path = require("path");
const fs = require("fs");

const executingDir = fs.realpathSync(process.cwd());
const dist = path.resolve(executingDir, "dist");

module.exports = {
  mode: "production",
  entry: "./pkg/tlang-wasm.js",
  output: {
    path: dist,
    filename: "tlang-wasm.js",
    library: {
      name: 'Tlang',
      type: 'window'
    }
  },
  experiments: {
    asyncWebAssembly: true
  },
};

"use strict";

// Makes the script crash on unhandled rejections instead of silently
// ignoring them. In the future, promise rejections that are not handled will
// terminate the Node.js process with a non-zero exit code.
process.on("unhandledRejection", (err) => {
  throw err;
});

const { spawn } = require("child_process");
const path = require("path");
const fs = require("fs");

const execDir = fs.realpathSync(process.cwd());
const args = process.argv.slice(2);
const scriptIndex = args.findIndex((x) => x === "debug" || x === "pack");
const script = scriptIndex === -1 ? args[0] : args[scriptIndex];

if (["debug", "pack"].includes(script)) {
  const webpackCli = require.resolve(
    path.resolve(execDir, "node_modules", ".bin", "webpack")
  );
  const args = [webpackCli, "--config"];

  const buildProcess = spawn(
    process.execPath,
    args.concat(path.resolve(execDir, "config", `${script}.config.js`)),
    {
      cwd: execDir,
      stdio: 'inherit'
    }
  );

  buildProcess.on('close', (code, signal) => {
    if (signal === "SIGKILL") {
      console.log(
        "The build failed because the process exited too early. " +
          "This probably means the system ran out of memory or someone called " +
          "`kill -9` on the process."
      );
    } else if (signal === "SIGTERM") {
      console.log(
        "The build failed because the process exited too early. " +
          "Someone might have called `kill` or `killall`, or the system could " +
          "be shutting down."
      );
    }

    process.exit(code);
  });
} else {
  console.log('Unknown script "' + script + '".');
}

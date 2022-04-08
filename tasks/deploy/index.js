const fs = require("fs");
const path = require("path");

const run = require("./run");
const walkDir = require("./utils/walkDir");

process.on("uncaughtException", (err, origin) => {
  console.error(err, origin);
});

const args = process.argv.slice(2);

if (args.length !== 1) {
  console.log("can only upload 1 directory");
  process.exit(1);
}

const package = args[0];
if (!fs.existsSync(package)) {
  console.log(`The package "${package}", does not exist.`);
  console.log("Please ensure relative paths are correct.");
  console.log(`Current working directory: ${process.cwd()}`);
  process.exit(1);
}

// this is used to ensure all binary files (e.g. just wasm right now) use the proper encoding
const encoding = (file) => (/\.wasm$/.test(file) ? "base64" : "utf-8");

const absolutePath = `${
  path.isAbsolute(package) ? package : path.resolve(package)
}${path.sep}`;
const files = walkDir(absolutePath).then((files) =>
  files.map((file) => ({
    path: file,
    blob: {
      path: file.replace(absolutePath, ""),
      encoding: encoding(file),
    },
  }))
);

const token = process.env.ACCESS_TOKEN;
const repo = process.env.REPO;
const user = "Thomas-Langlais";
const email = "thomas1446@gmail.com";

if (!token) {
  console.log(
    'A github access token must be provided from the environment variable "ACCESS_TOKEN"'
  );
  process.exit(1);
}
if (!repo) {
  console.log(
    'A repo name must be provided from the environment variable "REPO"'
  );
  process.exit(1);
}

run(user, email, repo, token, files);

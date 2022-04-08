const { promisify } = require("util");
const { readFile } = require("fs");

const read = promisify(readFile);

const { Octokit } = require("@octokit/rest");

async function run(owner, email, repo, accessToken, files) {
  const octokit = new Octokit({
    auth: accessToken,
    log: {
      warn: console.warn,
      error: console.error,
    },
  });

  const toBlobs = async (files) => {
    return await Promise.all(
      files.map(async (file) => {
        const { path, encoding } = file.blob;
        const response = await octokit.git.createBlob({
          owner,
          repo,
          encoding,
          content: (await read(file.path)).toString(encoding),
        });
        if (response.status !== 201)
          throw new Error(`Could not create the blob for ${file.path}`);

        return {
          path,
          sha: response.data.sha,
        };
      })
    );
  };

  const refResponse = await octokit.git.getRef({
    owner,
    repo,
    ref: "heads/main",
  });
  if (refResponse.status !== 200)
    throw new Error("Could not find the main branch ref");

  const head = refResponse.data;
  const lastCommitSha = head.object.sha;

  files = await files;
  const packageIndex = files.findIndex(
    (file) => file.blob.path === "package.json"
  );
  if (packageIndex === -1)
    throw new Error("Needs a package.json to set the tag version");

  const package = require(files[packageIndex].path);
  const { version } = package;
  if (typeof version !== "string")
    throw new Error("the package version was not supplied");

  const blobs = await toBlobs(files);
  const treeResponse = await octokit.git.createTree({
    owner,
    repo,
    tree: blobs.map((blob) => ({
      path: blob.path,
      mode: "100644",
      sha: blob.sha,
    })),
  });
  if (treeResponse.status !== 201)
    throw new Error(
      `Could not create the commit tree ${
        treeResponse.status
      }\n${JSON.stringify(treeResponse.headers)}`
    );

  const tree = treeResponse.data;
  const newTreeSha = tree.sha;

  let newCommitResponse = await octokit.git.createCommit({
    owner,
    repo,
    parents: [lastCommitSha],
    tree: newTreeSha,
    message: `Version release commit v${version}`,
    author: {
      name: owner,
      email: email,
    },
  });
  if (newCommitResponse.status !== 201)
    throw new Error(
      `Could not commit ${newCommitResponse.status}\n${JSON.stringify(
        newCommitResponse.headers
      )}\n${JSON.stringify(newCommitResponse.data)}`
    );

  const newCommit = newCommitResponse.data;
  const newCommitSha = newCommit.sha;

  const updateRefResponse = await octokit.git.updateRef({
    owner,
    repo,
    ref: "heads/main",
    sha: newCommitSha,
  });
  if (updateRefResponse.status !== 200)
    throw new Error("could not update the 'main' branch");

  const tagObjectResponse = await octokit.git.createTag({
    owner,
    repo,
    tag: `v${version}`,
    message: `tlang wasm release v${version}`,
    type: "commit",
    object: newCommitSha,
    tagger: {
      name: owner,
      email: email,
    },
  });
  if (tagObjectResponse.status !== 201)
    throw new Error("could not create the tag object");

  const tagObject = tagObjectResponse.data;
  const tagSha = tagObject.sha;

  const newTagResponse = await octokit.git.createRef({
    owner,
    repo,
    ref: `refs/tags/v${version}`,
    sha: tagSha,
  });
  if (newTagResponse.status !== 201)
    throw new Error("could not create tag reference");

  console.log(`Successfully created tag v${version}!`);
  console.log(`Link ${newTagResponse.data.url}`);
}

module.exports = run;

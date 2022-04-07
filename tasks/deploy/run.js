const { promisify } = require("util");
const { readFile } = require("fs");

const read = promisify(readFile);

const { Octokit } = require("@octokit/rest");

async function run(owner, email, repo, accessToken, filePromise) {
  const octokit = new Octokit({
    auth: accessToken,
    log: {
      warn: console.warn,
      error: console.error,
    },
  });

  const pushBlobs = async (filePromise) => {
    let files = await filePromise;

    let blobResults = await Promise.all(
      files.map(async (file) => ({
        ...file,
        response: await octokit.git.createBlob({
          owner,
          repo,
          content: await read(file.path, { encoding: "utf8" }),
        }),
      }))
    );

    let blobs = blobResults.map((blob) => {
      if (blob.response.status !== 201)
        throw new Error(
          `blob ${blob.path} was not created.\n${JSON.stringify(
            blob.response
          )}`
        );
      return {
        path: blob.blobPath,
        sha: blob.response.data.sha,
      };
    });

    console.log(blobs);
    return blobs;
  };

  const blobResults = pushBlobs(filePromise);

  const refResponse = await octokit.git.getRef({
    owner,
    repo,
    ref: "heads/main",
  });
  if (refResponse.status !== 200)
    throw new Error("Could not find the main branch ref");

  const head = refResponse.data;
  const lastCommitSha = head.object.sha;

  const blobs = await blobResults;

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
    message: "new release commit!",
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
    ref: 'heads/main',
    sha: newCommitSha
  });
  if (updateRefResponse.status !== 200)
    throw new Error("could not update the 'main' branch");
}

module.exports = run;

I was encoding the wasm file with utf8, which caused it to mangle the output

Test code
```javascript
function humanFileSize(size) {
  var i = Math.floor( Math.log(size) / Math.log(1024) );
  return ( size / Math.pow(1024, i) ).toFixed(2) * 1 + ' ' + ['B', 'kB', 'MB', 'GB', 'TB'][i];
};

const fileStats = fs.statSync(package);
const startingSize = fileStats.size;
console.log(`starting size of ${package} - ${humanFileSize(fileStats.size)}`);

const octokit = new Octokit({
  auth: token,
  log: {
    warn: console.warn,
    error: console.error,
  },
});

(async () => {
  const b64Response = await octokit.git.createBlob({
    owner: user,
    repo,
    content: fs.readFileSync(package).toString('base64'),
    encoding: 'base64'
  });
  const b64Sha = b64Response.data.sha;
  
  
  const utf8Response = await octokit.git.createBlob({
    owner: user,
    repo,
    content: fs.readFileSync(package).toString('utf-8'),
    encoding: 'utf-8'
  });
  const utf8Sha = utf8Response.data.sha;

  const blobb64Response = await octokit.git.getBlob({
    owner: user,
    repo,
    file_sha: b64Sha,
    mediaType: {
      format: 'raw'
    }
  });
  const b64Name = `base64Output-${path.basename(package)}`;
  fs.writeFileSync(b64Name, Buffer.from(blobb64Response.data));

  const b64Stats = fs.statSync(b64Name);
  console.log(`base64 size ${humanFileSize(b64Stats.size)}`);
  console.log(`It does${startingSize === b64Stats.size ? '' : ' not'} match!`);

  const blobutf8Response = await octokit.git.getBlob({
    owner: user,
    repo,
    file_sha: utf8Sha,
    mediaType: {
      format: 'raw'
    }
  });
  const utf8Name = `utf8Output-${path.basename(package)}`;
  fs.writeFileSync(utf8Name, blobutf8Response.data, 'utf-8');

  const utf8Stats = fs.statSync(utf8Name);
  console.log(`utf8 size ${humanFileSize(utf8Stats.size)}`);
  console.log(`It does${startingSize === utf8Stats.size ? '' : ' not'} match!`);
})();
```

executing this results with this output
```
starting size of ../../wasm/pkg/tlang-wasm_bg.wasm - 78.53 kB
base64 size 78.53 kB
It does match!
utf8 size 90.51 kB
It does not match!
```

So I have to ensure I encode all binary objects into base64 representation or else the files will be incorrect
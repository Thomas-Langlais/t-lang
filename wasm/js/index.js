import init, { run } from "../pkg/tlang-wasm";

init()
  .then(() => {
    document.getElementById("input").addEventListener("keyup", (e) => {
      if (e.key === "Enter" || e.keyCode === 13) {
        const input = `${e.target.value}\n`;
        const result = run(input);

        document.getElementById("output").innerText = result;

        e.target = "";
      }
    });
  })
  .catch(console.err);

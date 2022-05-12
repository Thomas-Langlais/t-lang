import "../css/index.css"

import { start, TerminalApi } from "../pkg/tlang_wasm";

document.body.innerHTML = `<!-- <input type="text" id="input"> -->
<textarea id="input"></textarea>
<p>Result:</p>
<div style="white-space: pre; font-family: monospace;" id="code"></div>
<div style="white-space: pre; font-family: monospace;" id="history"></div>`

start();

const terminal = new TerminalApi(document.getElementById("code"));
const keyboardInterface = terminal.browser_keyboard_interface();
const outputInterface = terminal.output_interface();

setInterval(() => {
  const output = outputInterface.poll();
  if (output) {
    const history = document.getElementById("history");
    
    const code = document.createElement("p");
    code.textContent = output.code;
    history.prepend(code);
    const result = document.createElement("p");
    result.textContent = output.result;
    history.prepend(result);

    document.getElementById("input").value = "";
    output.free()
  }
}, 50);

terminal.run_repl_loop();

document.getElementById("input").addEventListener("keydown", (e) => {
  if (e.repeat) return;
  keyboardInterface.send_keyboard_event(e);
  e.preventDefault();
});
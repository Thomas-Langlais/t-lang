import "../css/index.css"

import { start, TerminalApi } from "../pkg/tlang_wasm";

document.body.innerHTML = `<!-- <input type="text" id="input"> -->
<textarea id="input"></textarea>
<p>Result:</p>
<div style="white-space: pre; font-family: monospace;" id="code"></div>
<div style="white-space: pre; font-family: monospace;" id="output"></div>`

start();

const terminal = new TerminalApi(document.getElementById("code"));
const keyboardInterface = terminal.browser_keyboard_interface();
const outputInterface = terminal.output_interface();

setInterval(() => {
  const result = outputInterface.poll();
  if (result) {
    document.getElementById("output").textContent = result;
    document.getElementById("input").value = "";
  }
}, 50);

terminal.run_repl_loop();

document.getElementById("input").addEventListener("keydown", (e) => {
  let handle_keypress = keyboardInterface.send_keyboard_event(e);
  if (!handle_keypress)
    e.preventDefault();
});
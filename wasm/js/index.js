import init, {run} from '../pkg/tlang-wasm.js';

init().then(_ => {
    document.getElementById('input').addEventListener('keyup', (e) => {
        if (e.key === 'Enter' || e.keyCode === 13) {
            const input = `${e.target.value}\n`;

            const result = run(input);

            document.getElementById('output').innerText = result;

            input.value = '';
        }
    })
}).catch(console.error);
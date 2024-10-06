window.onload = () => {
    const history_limit = 10;
    const code_input = document.getElementById("code_input");
    const text_input = document.getElementById("text_input");
    const code_display = document.getElementById("code_display");
    const data_display = document.getElementById("data_display");
    const output_display = document.getElementById("output_display");

    let save = null;
    let history = [];
    let code;
    let input;
    let state; 
    let running;

    const reset = () => {
        code = code_input.value;
        input = text_input.value;
        state = {node: parse(code), data: [0], data_pos: 0, output: "", input_pos: 0};
        running = false;

        update();
        console.log(state);
    }

    const update = () => {
        code_display.innerHTML =
            code.substring(0, state.node.pos) +
            "<strong>" + code[state.node.pos] + "</strong>" +
            code.substring(state.node.pos + 1, code.length);

        let counter = 0;
        let d = state.data.map(n => {
            let a = n.toString();
            if (a.length < 3) {
                a = "0" + a;
            }
            if (a.length < 3) {
                a = "0" + a;
            }
            if (counter === state.data_pos) {
                a = "<strong>" + a + "</strong>";
            }
            if (counter % 4 === 0) {
                a = "<br/>" + a;
            }
            counter++;
            return a;
        });

        data_display.innerHTML = d.join(" ");

        output_display.innerText = state.output;
    }

    const parse = (s) => {
        let tokens = [];
        let string_pos = 0;
        while (s.length > string_pos) {
            const c = s[string_pos];
            if ("!+-<>[],.".includes(c)) {
                tokens.push({token: c, pos: string_pos});
            }
            string_pos++;
        }
        tokens.push({token: "!", pos: -1});
        console.log(Array.from(tokens));

        const children = parse_inner(tokens);
        const parent = {type: null, pos: -1, children, index: 0};
        parent.parent = parent; // words
        for (let i = 0; i < children.length; i++) {
            children[i].parent = parent;
        }
        return parent.children[0];
    }

    const parse_inner = (tokens) => {
        let nodes = [];
        while (tokens.length > 0 && tokens[0].token !== "]") {
            const t = tokens.shift();
            if (t.token === "[") {
                const children = parse_inner(tokens);
                const parent = {type: "[", pos: t.pos, children, index: children.length};
                nodes.push(parent);
                for (let i = 0; i < children.length; i++) {
                    children[i].parent = parent;
                }
                tokens.shift();
            } else {
                nodes.push({type: t.token, pos: t.pos});
            }
        }
        return nodes;
    }

    const sanitize = (s) => {
        return s.replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;").replaceAll('"', "&quot;").replaceAll("'", "&#039;");
    }

    const step = () => {
        history.push(structuredClone(state));
        if (history.length > history_limit) {
            history.shift();
        }

        if (state.node.type === "+") {
            state.data[state.data_pos]++;
            state.data[state.data_pos] %= 256;
        } else if (state.node.type === "-") {
            state.data[state.data_pos] += 255;
            state.data[state.data_pos] %= 256;
        } else if (state.node.type === "<") {
            state.data_pos--
        } else if (state.node.type === ",") {
            if (state.input_pos >= input.length) {
                state.data[state.data_pos] = 0;
            } else {
                state.data[state.data_pos] = input.charCodeAt(state.input_pos);
                state.input_pos++;
            }
        } else if (state.node.type === ".") {
            state.output = state.output + String.fromCharCode(state.data[state.data_pos]);
        } else if (state.node.type === ">") {
            state.data_pos++;
            if (state.data_pos === state.data.length) {
                state.data.push(0);
            }
        } else if (state.node.type === "[") {
            if (state.data[state.data_pos] > 0) {
                state.node.index = 0;
            }
        } else if (state.node.type === "!") {
            running = false;
        }

        if (!(("children" in state.node) && (state.node.index < state.node.children.length))) {
            state.node = state.node.parent;
            state.node.index++;
        }
        while ("children" in state.node) {
            if (state.node.index < state.node.children.length) {
                state.node = state.node.children[state.node.index];
            } else {
                break;
            }
        }
        update();
    }

    reset();
    document.getElementById("reset").onclick = reset;
    document.getElementById("step").onclick = step;
    document.getElementById("run").onclick = () => {running = true};
    document.getElementById("stop").onclick = () => {running = false};
    document.getElementById("back").onclick = () => {state = history.pop(); update()};

    setInterval(() => {
        if (running) {
            step();
        }
    }, 0);
}

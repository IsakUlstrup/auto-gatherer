import { Elm } from "./src/Main.elm";

const windowSize = { width: window.innerWidth, height: window.innerHeight }

Elm.Main.init({ node: document.getElementById("app"), flags: windowSize });


import { Elm } from "./src/Main.elm";

const screenSize = { width: window.innerWidth, height: window.innerHeight }

Elm.Main.init({ node: document.getElementById("app"), flags: screenSize });


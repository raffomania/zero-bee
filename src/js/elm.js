import { Elm } from "../Main.elm";

export function initElm() {
    let app = Elm.Main.init({
        node: document.getElementById("elm"),
    });

    return app;
}

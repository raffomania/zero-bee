import { Elm } from "./Main.elm";

import { save } from "@tauri-apps/api/dialog";
import { writeText } from "@tauri-apps/api/clipboard";

window.addEventListener("DOMContentLoaded", () => {
    let app = Elm.Main.init({
        node: document.getElementById("elm"),
    });

    app.ports.downloadBackup.subscribe(() => {
        save().then(console.log);
    });
});

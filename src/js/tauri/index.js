import { initElm } from "../elm";

import { save } from "@tauri-apps/api/dialog";

window.addEventListener("DOMContentLoaded", () => {
    let app = initElm();

    app.ports.downloadBackup.subscribe(() => {
        var year = new Date().getFullYear();
        var month = parseInt(new Date().getMonth(), 10) + 1;

        save({
            defaultPath: `zero_bee_backup_${year}_${month}.json`,
            filters: [{ name: "JSON Files", extensions: ["json"] }],
        }).then(console.log);
    });
});

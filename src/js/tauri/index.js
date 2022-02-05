import { initElm } from "../elm";
import { initRemoteStorage } from "../remotestorage";
import download from "./download";

window.addEventListener("DOMContentLoaded", () => {
    const app = initElm();
    initRemoteStorage(app).then((client) => {
        app.ports.downloadBackup.subscribe(() => {
            client.getFile("db.json").then(download);
        });
    });
});

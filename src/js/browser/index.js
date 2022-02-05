import { initElm } from "../elm";
import download from "./download";
import { initRemoteStorage } from "../remotestorage";

window.addEventListener("DOMContentLoaded", () => {
    const app = initElm();
    initRemoteStorage(app).then((client) => {
        app.ports.downloadBackup.subscribe(() => {
            client.getFile("db.json").then(download);
        });
    });
});

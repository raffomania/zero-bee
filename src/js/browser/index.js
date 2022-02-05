import RemoteStorage from "remotestoragejs";
import { initElm } from "../elm";
import debounce from "../debounce";
import download from "./download";

const remoteStorage = new RemoteStorage({ logging: true });
remoteStorage.access.claim("whynab", "rw");
remoteStorage.caching.enable("/whynab/");

window.addEventListener("DOMContentLoaded", () => {
    remoteStorage.on("ready", () => {
        const client = remoteStorage.scope("/whynab/");

        let app = initElm();

        app.ports.sendModel.subscribe(
            debounce((data) => {
                client.storeFile(
                    "application/json",
                    "db.json",
                    JSON.stringify(data)
                );
            }, 1000)
        );

        app.ports.connect.subscribe((address) => {
            remoteStorage.connect(address);
        });

        app.ports.downloadBackup.subscribe(() => {
            client.getFile("db.json").then(download);
        });

        client.on("change", (event) => {
            if (event.origin === "conflict") {
                console.error("conflict", event);
                alert("remotestorage conflict!");
                return;
            }
            app.ports.modelUpdated.send(event.newValue);
        });
    });
});

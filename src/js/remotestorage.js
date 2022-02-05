import RemoteStorage from "remotestoragejs";
import debounce from "./debounce";

export function initRemoteStorage(elmApp) {
    const remoteStorage = new RemoteStorage({ logging: true });
    remoteStorage.access.claim("whynab", "rw");
    remoteStorage.caching.enable("/whynab/");

    return new Promise((resolve, _reject) => {
        remoteStorage.on("ready", () => {
            const client = connectRemoteStorage(remoteStorage, elmApp);
            resolve(client);
        });
    });
}

function connectRemoteStorage(remoteStorage, elmApp) {
    const client = remoteStorage.scope("/whynab/");

    elmApp.ports.sendModel.subscribe(
        debounce((data) => {
            client.storeFile(
                "application/json",
                "db.json",
                JSON.stringify(data)
            );
        }, 1000)
    );

    elmApp.ports.connect.subscribe((address) => {
        remoteStorage.connect(address);
    });

    client.on("change", (event) => {
        if (event.origin === "conflict") {
            console.error("conflict", event);
            alert("remotestorage conflict!");
            return;
        }
        elmApp.ports.modelUpdated.send(event.newValue);
    });

    return client;
}

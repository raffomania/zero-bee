import { Elm } from './Main.elm';
import RemoteStorage from 'remotestoragejs';
import Widget from 'remotestorage-widget';

const remoteStorage = new RemoteStorage({logging: true});
remoteStorage.access.claim('whynab', 'rw');
remoteStorage.caching.enable('/whynab/');

window.addEventListener('DOMContentLoaded', () => {
    const widget = new Widget(remoteStorage);
    widget.attach('remotestorage');

    remoteStorage.on('ready', () => {
        const client = remoteStorage.scope('/whynab/');
        let app = Elm.Main.init({
            node: document.getElementById('elm'),
        });

        app.ports.sendModel.subscribe((data) => {
            client.storeFile('application/json', 'db.json', JSON.stringify(data));
        });

        client.on('change', (event) => {
            if (event.origin === 'conflict') {
                console.error('conflict', event);
            }
            app.ports.modelUpdated.send(event.newValue);
        });
    });
});

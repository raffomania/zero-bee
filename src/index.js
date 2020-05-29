import { Elm } from './Main.elm';
import RemoteStorage from 'remotestoragejs';
import Widget from 'remotestorage-widget';

const remoteStorage = new RemoteStorage({logging: true});
remoteStorage.access.claim('whynab', 'rw');
remoteStorage.caching.enable('/whynab/');

window.addEventListener('DOMContentLoaded', () => {
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

    customElements.define('connect-remote-storage',
        class extends HTMLElement {
            constructor() { 
                super(); 
                this.widget = new Widget(remoteStorage, {modalBackdrop: false});
            }
            connectedCallback() { 
                this.setAttribute('id', 'widget-container');
                this.widget.attach('widget-container')
            }
            attributeChangedCallback() {}
            static get observedAttributes() {}
        }
    );
});

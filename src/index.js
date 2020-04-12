import { Elm } from './Main.elm';
import RemoteStorage from 'remotestoragejs';
import Widget from 'remotestorage-widget';

const remoteStorage = new RemoteStorage({logging: true});
remoteStorage.access.claim('whynab', 'rw');
remoteStorage.caching.enable('/whynab/');

window.addEventListener('DOMContentLoaded', () => {
    Elm.Main.init({
        node: document.getElementById('elm')
    });

    const widget = new Widget(remoteStorage);
    widget.attach('remotestorage');

    remoteStorage.on('ready', () => {
        console.log('ready');
    });
});

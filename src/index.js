import { Elm } from './Main.elm';
import RemoteStorage from 'remotestoragejs';
import Theme from './theme.js';

const remoteStorage = new RemoteStorage({logging: true});
remoteStorage.access.claim('whynab', 'rw');
remoteStorage.caching.enable('/whynab/');

window.addEventListener('DOMContentLoaded', () => {
    const theme = new Theme();
    theme.install(document.body);
    theme.start();

    remoteStorage.on('ready', () => {
        const client = remoteStorage.scope('/whynab/');
        let app = Elm.Main.init({
            node: document.getElementById('elm'),
        });

        app.ports.sendModel.subscribe(debounce((data) => {
            console.warn('store');
            client.storeFile('application/json', 'db.json', JSON.stringify(data));
        }, 1000));

        app.ports.connect.subscribe((address) => {
            remoteStorage.connect(address);
        });

        client.on('change', (event) => {
            if (event.origin === 'conflict') {
                console.error('conflict', event);
                alert('remotestorage conflict!');
                return;
            }
            app.ports.modelUpdated.send(event.newValue);
        });
    });
});

// stolen from underscore.js
// Returns a function, that, as long as it continues to be invoked, will not
// be triggered. The function will be called after it stops being called for
// N milliseconds. If `immediate` is passed, trigger the function on the
// leading edge, instead of the trailing.
function debounce(func, wait, immediate) {
	var timeout;
	return function() {
		var context = this, args = arguments;
		var later = function() {
			timeout = null;
			if (!immediate) func.apply(context, args);
		};
		var callNow = immediate && !timeout;
		clearTimeout(timeout);
		timeout = setTimeout(later, wait);
		if (callNow) func.apply(context, args);
	};
};

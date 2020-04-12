import { Elm } from './Main.elm';

window.addEventListener('DOMContentLoaded', function() {
    Elm.Main.init({
        node: document.getElementById('elm')
    });
});

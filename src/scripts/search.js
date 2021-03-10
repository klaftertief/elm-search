import { Elm } from "../../elm-stuff/generated-code/klaftertief/elm-search/Main.elm";

var app = Elm.Main.init({
        node: document.getElementById('main'),
        flags: {
            search: window.location.search || ""
        }
    }
);

window.onpopstate = function (event) {
    app.ports.query.send(window.location.search);
};
app.ports.pushQuery.subscribe(function (query) {
    window.history.pushState({}, "", query);
    app.ports.query.send(window.location.search);
});

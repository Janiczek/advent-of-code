const someDependency = require('some-dependency');
const Elm = require('../js/201X-XX.js');
const app = Elm.Year201X.DayXX.worker();
app.ports.elmAsks.subscribe(data => app.ports.jsAnswers.send(someDependency(data)));

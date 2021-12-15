const md5 = require('md5-jkmyers');
const Elm = require('../js/2016-05.js');
const app = Elm.Year2016.Day05.worker();
app.ports.elmAsks.subscribe(string => app.ports.jsAnswers.send(md5(string)));

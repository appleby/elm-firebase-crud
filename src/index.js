'use strict';

require('font-awesome/css/font-awesome.css');

require('./index.html');

var firebase = require("firebase");
var firebase_config = require("../firebase-app-config.json");
firebase.initializeApp(firebase_config);

var Elm = require('./Main.elm');
var app = Elm.Main.fullscreen();

var ports = require('./ports.js');
ports.subscribe(app, firebase);

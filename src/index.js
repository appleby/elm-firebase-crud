'use strict';

require('font-awesome/css/font-awesome.css');

require('./index.html');

var firebase = require("firebase");
var firebase_config = require("../firebase-app-config.json");
firebase.initializeApp(firebase_config);

function signIn() {
    var provider = new firebase.auth.GoogleAuthProvider();
    firebase.auth().signInWithPopup(provider)
        .then(function(userCred) {
            app.ports.signInOk.send(userCred);
        })
        .catch(function(error) {
            console.log(error);
            let authErr = {
                code: error.code || null,
                message: error.message || null
            };
            app.ports.signInErr.send(authErr);
        });
}

function signOut() {
    firebase.auth().signOut()
        .then(function() {
            app.ports.signOutOk.send(true);
        })
        .catch(function(error) {
            console.log(error);
            app.ports.signOutOk.send(false);
        });
}

function onAuthStateChanged(user) {
    if (!user) {
        app.ports.signOutOk.send(true);
    }
}
firebase.auth().onAuthStateChanged(onAuthStateChanged);

var Elm = require('./Main.elm');
var app = Elm.Main.fullscreen();

app.ports.signIn.subscribe(signIn);
app.ports.signOut.subscribe(signOut);

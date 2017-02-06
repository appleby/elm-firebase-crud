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

function taskOp(fn) {
    return function() {
        let user = firebase.auth().currentUser;
        if (!user) {
            console.log(fn.name, ": currentUser is null");
            return;
        }
        let path = "test/" + user.uid + "/tasks";
        let ref = firebase.database().ref(path);
        let args = [ref].concat(Array.prototype.slice.call(arguments));
        fn.apply(null, args);
    }
}

function fetchTasks(taskRef) {
    taskRef.once("value")
        .then(function(snap) {
            app.ports.fetchTasksOk.send(snap.val());
        })
        .catch(function(error) {
            console.log(error);
        });
}

function addTask(taskRef, task) {
    taskRef.push(task)
        .then(function(newRef) {
            app.ports.addTaskOk.send(true);
        })
        .catch(function(error) {
            console.log(error);
            app.ports.addTaskOk.send(false);
        });
}

function deleteTask(taskRef, taskId) {
    let ref = taskRef.child(taskId);
    ref.remove()
        .then(function() {
            app.ports.deleteTaskOk.send(true);
        })
        .catch(function(error) {
            console.log(error);
            app.ports.deleteTaskOk.send(false);
        });
}

function saveTask(taskRef, task) {
    let ref = taskRef.child(task.id);
    delete task.id;
    ref.set(task)
        .then(function() {
            app.ports.saveTaskOk.send(true);
        })
        .catch(function(error) {
            console.log(error);
            app.ports.saveTaskOk.send(false);
        });
}

var Elm = require('./Main.elm');
var app = Elm.Main.fullscreen();

app.ports.signIn.subscribe(signIn);
app.ports.signOut.subscribe(signOut);
app.ports.fetchTasks.subscribe(taskOp(fetchTasks));
app.ports.addTaskPort.subscribe(taskOp(addTask));
app.ports.deleteTask.subscribe(taskOp(deleteTask));
app.ports.saveTaskPort.subscribe(taskOp(saveTask));

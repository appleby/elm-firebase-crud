'use strict';

const functions = require('firebase-functions');
const admin = require('firebase-admin');

const data = require("./db-data.json");

admin.initializeApp(functions.config().firebase);

exports.populateUserData = functions.auth.user().onCreate(function(event) {
    const uid = event.data.uid;
    const tasksRef = admin.database().ref("/users/" + uid + "/tasks");
    return Promise.all(data.tasks.map(function(task) {
        let newRef = tasksRef.push();
        task.id = newRef.key;
        return newRef.set(task)
            .then(function() {
                console.log("Added task " + newRef.key);
            })
            .catch(function(error) {
                console.error("Failed to add task " + newRef.key + ": " + error);
            });
    }));
});

exports.cleanupUserData = functions.auth.user().onDelete(function(event) {
    const uid = event.data.uid;
    return admin.database().ref("/users/" + uid).remove();
});

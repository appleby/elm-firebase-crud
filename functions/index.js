'use strict';

const functions = require('firebase-functions');
const admin = require('firebase-admin');
admin.initializeApp();

const data = require("./db-data.json");
const deleteAccounts = require("./delete-unused-accounts-cron")(admin);

exports.deleteUserAccounts = deleteAccounts.accountcleanup;

exports.populateUserData = functions.auth.user().onCreate(function(user) {
    const tasksRef = admin.database().ref("/users/" + user.uid + "/tasks");
    return Promise.all(data.tasks.map(function(task) {
        let newRef = tasksRef.push();
        task.id = newRef.key;
        return newRef.set(task)
            .then(function() {
                console.log("Added task " + newRef.key);
            });
    }));
});

exports.cleanupUserData = functions.auth.user().onDelete(function(user) {
    return admin.database().ref("/users/" + user.uid).remove();
});

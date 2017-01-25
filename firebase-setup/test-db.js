'use-strict';

var admin = require("firebase-admin");
var cert = require("./timeslots-61887-firebase-adminsdk-yikti-7f735e254c.json");
var data = require("./init-db.json");

admin.initializeApp({
    credential: admin.credential.cert(cert),
    databaseURL: "https://timeslots-61887.firebaseio.com",
    databaseAuthVariableOverride: {
        uid: "test-db-setup"
    }
});

function die(message) {
    console.log(message);
    process.exit(1);
}

function waituntil(done) {
    return function() {
        if (done()) {
            console.log("DB setup complete");
            return;
        }
        setTimeout(waituntil(done), 500);
    }
}

var db = admin.database();
var testdb = db.ref("/test");

testdb.remove()
    .then(function () {
        let wait = 0;
        let tasksref = testdb.child("tasks");
        data.tasks.forEach(function(task) {
            wait++;
            let newtask = tasksref.push(task);
            newtask.then(function() {
                console.log("Added task " + newtask.key);
                wait--;
            })
            .catch(function(error) {
                die("Failed to add task " + newtask.key + ": " + error.message)
            });
        });
        setTimeout(waituntil(() => wait == 0), 500);
    })
    .catch(function(error) {
        die("Failed to remove /test: " + error.message);
    });
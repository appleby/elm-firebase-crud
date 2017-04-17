'use-strict';

var admin = require("firebase-admin");
var cert = require("./timeslots-61887-firebase-adminsdk-yikti-7f735e254c.json");
var data = require("./init-db.json");
var testConfig = require("./test-config.json");
var firebaseConfig = require("../firebase-app-config.json");

admin.initializeApp({
    credential: admin.credential.cert(cert),
    databaseURL: firebaseConfig.databaseURL,
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
	    // Don't exit here, or node-foreman will consider it an error.
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
        let tasksref = testdb.child(testConfig.userId.concat("/tasks"));
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

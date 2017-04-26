'use-strict';

const admin = require("firebase-admin");
const data = require("./db-data.json");
const cert = require("../secrets/firebase-adminsdk-service-account-private-key.json");
const config = require("../secrets/setup-test-db-config.json");
const firebaseConfig = require("../secrets/firebase-app-config.json");
const databaseRules = require("../database.rules.json")
const authUid = parseUidFromAuthRule(databaseRules.rules[".write"]);

admin.initializeApp({
    credential: admin.credential.cert(cert),
    databaseURL: firebaseConfig.databaseURL,
    databaseAuthVariableOverride: {
        uid: authUid,
    }
});

function die(message) {
    console.log(message);
    process.exit(1);
}

function parseUidFromAuthRule(rule) {
    var match = rule.match(/auth.uid\s*==\s*'(.+)'/);
    if (match === null || match.length < 2 || match[1] === null) {
	die("Unable to parse uid from auth rule.");
    }
    return match[1];
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

var db = admin.database().ref();

db.remove()
    .then(function () {
        let wait = 0;
        let tasksref = db.child("users/" + config.userId + "/tasks");
        data.tasks.forEach(function(task) {
            wait++;
            let newtaskref = tasksref.push();
            // Denormalize and store the id in the task as well, to
            // make encode/decode simpler in Ports.elm and ports.js.
            task.id = newtaskref.key;
            newtaskref.set(task).then(function() {
                console.log("Added task " + newtaskref.key);
                wait--;
            })
            .catch(function(error) {
                die("Failed to add task " + newtaskref.key + ": " + error.message)
            });
        });
        setTimeout(waituntil(() => wait == 0), 500);
    })
    .catch(function(error) {
        die("Failed to remove db root: " + error.message);
    });

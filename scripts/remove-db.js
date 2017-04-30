'use-strict';

const admin = require("firebase-admin");
const yesno = require("yesno");

const cert = require("../secrets/firebase-adminsdk-service-account-private-key.json");
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
    console.error(message);
    process.exit(1);
}

function exitOk(message) {
    console.log(message);
    process.exit(0);
}

function parseUidFromAuthRule(rule) {
    var match = rule.match(/auth.uid\s*==\s*'(.+)'/);
    if (match === null || match.length < 2 || match[1] === null) {
	die("Unable to parse uid from auth rule.");
    }
    return match[1];
}

yesno.ask("WARNING: This script will remove all data from the firebase DB at " +
          firebaseConfig.databaseURL + ".\nContinue? [y|N]",
          false,
          function(ok) {
              if (!ok) exitOk("Aborting.");
              console.log("Ok. Removing db root...");
              admin.database().ref().remove()
                  .then(function () {
                      exitOk("Removed db root.")
                  })
                  .catch(function(error) {
                      die("Failed to remove db root: " + error.message);
                  });
          }
         );

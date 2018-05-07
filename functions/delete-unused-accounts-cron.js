/* This file is derived from
 *
 * https://github.com/firebase/functions-samples/blob/master/delete-unused-accounts-cron/functions/index.js
 *
 * This version has been modified to delete accounts that have been
 * inactive for 30 minutes, rather than 30 days.
 *
 * It has also been modified to pass the admin module as an argument
 * to module.exports, to support importing this module from the main
 * index.js.
 */

/**
 * Copyright 2016 Google Inc. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for t`he specific language governing permissions and
 * limitations under the License.
 */
'use strict';

const functions = require('firebase-functions');
const promisePool = require('es6-promise-pool');
const PromisePool = promisePool.PromisePool;
const secureCompare = require('secure-compare');
// Maximum concurrent account deletions.
const MAX_CONCURRENT = 3;


module.exports = exports = function(admin) {
  var exports = {};
  /**
   * When requested this Function will delete every user accounts that has been inactive for 30 minutes.
   * The request needs to be authorized by passing a 'key' query parameter in the URL. This key must
   * match a key set as an environment variable using `firebase functions:config:set cron.key="YOUR_KEY"`.
   */
  exports.accountcleanup = functions.https.onRequest((req, res) => {
    const key = req.query.key;

    // Exit if the keys don't match.
    if (!secureCompare(key, functions.config().cron.key)) {
      console.log('The key provided in the request does not match the key set in the environment. Check that', key,
                  'matches the cron.key attribute in `firebase env:get`');
      res.status(403).send('Security key does not match. Make sure your "key" URL query parameter matches the ' +
                           'cron.key environment variable.');
      return null;
    }

    // Fetch all user details.
    return getInactiveUsers().then((inactiveUsers) => {
      // Use a pool so that we delete maximum `MAX_CONCURRENT` users in parallel.
      const promisePool = new PromisePool(() => deleteInactiveUser(inactiveUsers), MAX_CONCURRENT);
      return promisePool.start();
    }).then(() => {
      console.log('User cleanup finished');
      res.send('User cleanup finished');
      return null;
    });
  });

  /**
   * Deletes one inactive user from the list.
   */
  function deleteInactiveUser(inactiveUsers) {
    if (inactiveUsers.length > 0) {
      const userToDelete = inactiveUsers.pop();

      // Delete the inactive user.
      return admin.auth().deleteUser(userToDelete.uid).then(() => {
        console.log('Deleted user account', userToDelete.uid, 'because of inactivity');
        return null;
      }).catch(error => {
        console.error('Deletion of inactive user account', userToDelete.uid, 'failed:', error);
        return null;
      });
    }
    return null;
  }

  /**
   * Returns the list of all inactive users.
   */
  function getInactiveUsers(users = [], nextPageToken) {
    return admin.auth().listUsers(1000, nextPageToken).then((result) => {
      // Find users that have not signed in in the last 30 days.
      const inactiveUsers = result.users.filter(
        user => Date.parse(user.metadata.lastSignInTime) < (Date.now() - 30 * 60 * 1000));

      // Concat with list of previously found inactive users if there was more than 1000 users.
      users = users.concat(inactiveUsers);

      // If there are more users to fetch we fetch them.
      if (result.pageToken) {
        return getInactiveUsers(users, result.pageToken);
      }

      return users;
    });
  }

  return exports;
};

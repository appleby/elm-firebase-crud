# Overview

This repo contains an example [Elm](http://elm-lang.org/) todo-list application using [firebase](https://firebase.google.com/) for the backend. There is a live demo running at [https://elm-crud.firebaseapp.com](https://elm-crud.firebaseapp.com). The app doesn't do anything useful; It was just an excuse to try out Elm and firebase. You can log in anonymously by clicking the **Sign In** link in the navbar. After login, the task list is populated with some example data. All user accounts and data are automatically wiped 30 minutes after login.

The Elm code in this app communicates with the firebase client api via [Elm's javascript Ports](https://guide.elm-lang.org/interop/javascript.html). There is a nice-looking [elmfire] package for interacting with firebase, but, at the time of writing, it's in the process of being updated to work with Elm v0.18.

# Elm version

This demo application targets Elm v0.18. I have no plans to upgrade it to Elm v0.19 or above.

# Dev setup

## Clone and install npm dependencies

``` shell
git clone https://github.com/appleby/elm-firebase-crud.git
cd elm-firebase-crud
sudo npm install -g firebase-tools
npm install
cd functions; npm install; cd ..
```

## Init firebase

Create a new firebase project in the [firebase console][fbconsole], then login to firebase and select your project.

``` shell
firebase login
firebase use --add
```

For more info on how to create a new project, see the [firebase web setup docs][fbsetup].

## Install `firebase-app-config.json`

Next, save the firebase web configuration to `secrets/firebase-app-config.json`. This file is not really a "secret" since it's meant to be distributed with your app, but it's convenient to stick it in the `secrets` directory, which is included in `.gitignore`.

``` shell
mkdir secrets
firebase setup:web > secrets/firebase-app-config.json
chmod 600 secrets/firebase-app-config.json
```

Now edit the file `secrets/firebase-app-config.json`. Remove everything except the actual config object, making it a valid JSON file. The file should look something like this:

      {
        "apiKey": "<YOUR-API-KEY>",
        "databaseURL": "https://<YOUR-PROJECT>.firebaseio.com",
        "storageBucket": "<YOUR-PROJECT>.appspot.com",
        "authDomain": "<YOUR-PROJECT>.firebaseapp.com",
        "messagingSenderId": "<YOUR-MSG-ID>",
        "projectId": "<YOUR-PROJECT>"
      }

## Enable anonymous auth

This app uses the firebase anonymous auth provider. In the [firebase console][fbconsole], click on **Authentication** then **SIGN-IN METHOD** and enable the Anonymous provider.

## Test via localhost

First, deploy the firebase rules and cloud functions; then start the dev server.

``` shell
firebase deploy --only database,functions
npm run serve
```

The site should now be running at http://localhost:3000/.

## Deploy to firebase hosting

You can also deploy the site to firebase's hosting if you want it to be publicly accessible.

``` shell
npm run build
firebase deploy
```

The site should now be running at `https://<YOUR-PROJECT>.firebaseapp.com`.

## Cloud functions

This firebase project contains the following cloud functions:

| function name      | Triggered by   | description                                                                                                                                    |
|--------------------|----------------|------------------------------------------------------------------------------------------------------------------------------------------------|
| populateUserData   | account create | Populate the user's task list with example data on first login.                                                                                |
| cleanupUserData    | account delete | Delete all user data from the database on user account deletion.                                                                               |
| deleteUserAccounts | https          | Delete all user accounts with last login time > 30 minutes ago. Intended to be triggered from the third-party cron service, like [cron-job.org][cronjoborg]. |

### Enable running the `deleteUserAccounts` cloud function

The `deleteUserAccounts` cloud function is triggered via https, and will delete any user accounts with last login time > 30 minutes ago. This function is intended to be run from a third-party cron service like [cron-job.org][cronjoborg]. There is no need to set this up if you don't intend to keep a publicly-accessible demo running.

In order to run the `deleteUserAccounts` cloud function, you first need to configure the cron.key via the [firebase functions config env](https://firebase.google.com/docs/functions/config-env).

``` shell
npm install crypto
cronkey="$(node -e "console.log(require('crypto').randomBytes(20).toString('hex'))")"
firebase functions:config:set cron.key="$cronkey"
firebase deploy --only functions
```

The `firebase deploy` command should print the url to hit for the `deleteUserAccounts` functions. It will look something like:

    https://us-central1-<YOUR-PROJECT>.cloudfunctions.net/deleteUserAccounts

You can test that the function is working via `curl`, like so. Note the required `?key=` query parameter:

``` shell
curl https://us-central1-<YOUR-PROJECT>.cloudfunctions.net/deleteUserAccounts?key="$cronkey"
```

And check that there were no errors in the firebase logs.

``` shell
firebase functions:log
```

## Maintenance scripts

In the `scripts` directory, there is a `remove-db.js` script that will prompt before removing all data from the firebase database. To enable running the `remove-db.js` maintenance script, you first need to download a private key for the admin sdk service-account.

1. Navigate to the [Service Accounts](https://console.firebase.google.com/project/_/settings/serviceaccounts/adminsdk) tab of you project's settings page.
2. Select your project.
3. Click the **Generate New Private Key** button.
4. Save the private file to `secrets/firebase-adminsdk-service-account-private-key.json`
5. `chmod 600 secrets/firebase-adminsdk-service-account-private-key.json`

See the [firebase admin setup docs](https://firebase.google.com/docs/admin/setup#add_firebase_to_your_app) for more detailed instructions on setting up admin sdk access.

Once you've downloaded the admin sdk private key, you can run `npm run remove-db` or, equivalently, `node ./scripts/remove-db.js` to completely wipe the firebase database configured as the `databaseUrl` property in `secrets/firebase-app-config.json`.



[fbsetup]: https://firebase.google.com/docs/web/setup
[fbconsole]: https://console.firebase.google.com/
[elmfire]: https://github.com/ThomasWeiser/elmfire
[cronjoborg]: https://cron-job.org

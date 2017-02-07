'use strict';

exports.subscribe = function(app, firebase) {
    let signIn = function() {
        var provider = new firebase.auth.GoogleAuthProvider();
        firebase.auth().signInWithPopup(provider)
            .then(function(userCred) {
                app.ports.signInOk.send(userCred.user);
            })
            .catch(function(error) {
                console.log(error);
                let authErr = {
                    code: error.code || null,
                    message: error.message || null
                };
                app.ports.signInErr.send(authErr);
            });
    };

    let signOut = function() {
        firebase.auth().signOut()
            .then(function() {
                app.ports.signOutOk.send(true);
            })
            .catch(function(error) {
                console.log(error);
                app.ports.signOutOk.send(false);
            });
    };

    let onAuthStateChanged = function(user) {
        if (!user) {
            app.ports.signOutOk.send(true);
        }
    };

    let taskOp = function(fn) {
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
    };

    let fetchTasks = function(taskRef) {
        taskRef.once("value")
            .then(function(snap) {
                app.ports.fetchTasksOk.send(snap.val());
            })
            .catch(function(error) {
                console.log(error);
            });
    };

    let addTask = function(taskRef, task) {
        taskRef.push(task)
            .then(function(newRef) {
                app.ports.addTaskOk.send(true);
            })
            .catch(function(error) {
                console.log(error);
                app.ports.addTaskOk.send(false);
            });
    };

    let deleteTask = function(taskRef, taskId) {
        let ref = taskRef.child(taskId);
        ref.remove()
            .then(function() {
                app.ports.deleteTaskOk.send(true);
            })
            .catch(function(error) {
                console.log(error);
                app.ports.deleteTaskOk.send(false);
            });
    };

    let saveTask = function(taskRef, task) {
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
    };

    firebase.auth().onAuthStateChanged(onAuthStateChanged);
    app.ports.signIn.subscribe(signIn);
    app.ports.signOut.subscribe(signOut);
    app.ports.fetchTasks.subscribe(taskOp(fetchTasks));
    app.ports.addTaskPort.subscribe(taskOp(addTask));
    app.ports.deleteTask.subscribe(taskOp(deleteTask));
    app.ports.saveTaskPort.subscribe(taskOp(saveTask));
};

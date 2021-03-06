'use strict';

exports.subscribe = function(app, firebase) {
    let signIn = function() {
        firebase.auth().signInAnonymously()
            .catch(function(error) { console.log(error); });
    };

    let signOut = function() {
        firebase.auth().signOut();
    };

    let onAuthStateChanged = function(user) {
        app.ports.authStateChanged.send(user);
    };

    let taskOp = function(fn) {
        return function() {
            let user = firebase.auth().currentUser;
            if (!user) {
                console.log(fn.name, ": currentUser is null");
                return;
            }
            let path = "users/" + user.uid + "/tasks";
            let ref = firebase.database().ref(path);
            let args = [ref].concat(Array.prototype.slice.call(arguments));
            fn.apply(null, args);
        }
    };

    let sendValToPort = function(thenable, port) {
        thenable
            .then(function(snap) { port.send(snap.val()); })
            .catch(function(error) { console.log(error); });
    };

    let sendBoolToPort = function(thenable, port) {
        thenable
            .then(function() {
                port.send(true);
            })
            .catch(function(error) {
                console.log(error);
                port.send(false);
            });
    };

    let subscribeToUserTasks = function(tasksRef) {
        tasksRef.on("value", function(snap) {
            app.ports.userTasksOk.send(snap.val());
        });
    };

    let unSubscribeFromUserTasks = function(tasksRef) {
        tasksRef.off("value");
    };

    let fetchTask = function(tasksRef, taskId) {
        let taskRef = tasksRef.child(taskId);
        sendValToPort(taskRef.once("value"), app.ports.fetchTaskOk);
    };

    let addTask = function(tasksRef, task) {
        let taskRef = tasksRef.push();
        task.id = taskRef.key;
        sendBoolToPort(taskRef.set(task), app.ports.addTaskOk);
    };

    let deleteTask = function(tasksRef, taskId) {
        let taskRef = tasksRef.child(taskId);
        sendBoolToPort(taskRef.remove(), app.ports.deleteTaskOk);
    };

    let saveTask = function(tasksRef, task) {
        let taskRef = tasksRef.child(task.id);
        sendBoolToPort(taskRef.set(task), app.ports.saveTaskOk);
    };

    firebase.auth().onAuthStateChanged(onAuthStateChanged);
    app.ports.signIn.subscribe(signIn);
    app.ports.signOut.subscribe(signOut);
    app.ports.subscribeToUserTasks.subscribe(taskOp(subscribeToUserTasks));
    app.ports.unSubscribeFromUserTasks.subscribe(taskOp(unSubscribeFromUserTasks));
    app.ports.fetchTask.subscribe(taskOp(fetchTask));
    app.ports.addTaskPort.subscribe(taskOp(addTask));
    app.ports.deleteTask.subscribe(taskOp(deleteTask));
    app.ports.saveTaskPort.subscribe(taskOp(saveTask));
};

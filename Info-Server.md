# Running the server

To start the server, simple run 

    node Server.js [port] [save file]

The port is 8000 by default and the save file is `Traffic.js` by default.

## Using the commands

When you have started a server, you can type commands to run. There are a few commands and the syntax is inspired by SQL.

Commands:

### GRANT

    GRANT <ips> <permission>

When logging in, a user's IP is saved. This command grants a specific permission to the user with that IP. You can read more about permissions below.

### DENY

    DENY <ips> <permission>

This command is like the `GRANT` command, but instead of granting a permission, this instead removes it.

### QUERY

    QUERY <ips>

This command gives information about every IP specified. The output looks like this:

    QUERY *
      ::ffff:216.58.211.142
            name: "Google Car AI"
            perms: ["connect","view","place"]
### RM

    RM <ips>

This command makes the server forget that the specified IP's ever existed.

### MAKE

    MAKE <ip> <name>

This command creates a user with the specified IP and name. The IP doesn't necessarily have to be valid, but it's a bit useless to create a user with a invalid IP because then no one can use that user. You can use this to bypass the 15-character username limit

### SAVE

    SAVE [file]

This command is pretty straightforward, it just saves the current state to a file. If no file is given, it saves over the save file that the program started with.

WARNING: This command will override files without asking, so double-check that you won't remove anything you will need.

### SPAWNRATE

    SPAWNRATE <rate>

This sets the spawnrate to whatever you want it to be. The spawnrate is how often new cars will be added in seconds. The default is 2.

### DEBUG

    DEBUG [options]

This command turns on and off different loggings. The options can be any of `AI`, `ws`, `http` and `cmd`. If a specific options isn't set, it will be turned on and vice-versa.

Here's what every debug does:

* `AI`: (Not very useful) Logs different things that the AI sees / does. The output will get very crowded as this will create hundreds of logs every second.
* `ws`: Logs everything all the clients send through the WebSocket-protocol. This is all actions the user does.
* `http`: Logs every HTTP request and some info about it.
* `cmd`: Logs all commands ran.

Example:

    DEBUG ws
      Added ws
      Removed nothing
    Recieved remove/Car9 from ::1
    Recieved accel/6 from ::1
    Recieved accel/0 from ::1
    DEBUG ws
      Added nothing
      Removed ws
    DEBUG http
      Added ws, http
      Removed nothing
    GET /Textures/Cars/Car1.png
    GET /
    GET /Out/Main.js
    GET /favicon.ico
    GET /Textures/Cars/CarPolis.png
    GET /Textures/Cars/Car4.png
    DEBUG
      Current debugs: http
    DEBUG http blahg
      Invalid debug parameter blahg
      Added nothing
      Removed http

## Permissions:

You can give different clients different permissions with the `GRANT` and `DENY` commands. But what are these permissions?

Here's a list of all of them:

* `connect` (default): The ability to connect to the server. If a client doesn't have this permission the server will act like that user doesn't exist, it won't respond to `http` requests.
* `view` (default): The ability to view anything. This is close to the `connect` but instead of being completely ignored the user can load files and do http-requests, but they can't see the state of the map.
* `place` (default): The ability to place cars. This is pretty self-explanatory, but without it, the user will not see this button on the menu:
<img src="https://github.com/loovjo/Traffic/blob/master/Web/Textures/Buttons/AddCar.png" alt="Add car button" width="20px"></img>
* `police` (not default): The ability to place police cars. Police cars are similar to regular cars, but can go much faster and never breaks.
* `build` (not default): Access to building roads and placing traffic lights.
* `command` (not default): Access to run commands from the browser. This feature is not yet implemented fully, but users can send raw WebSocket requests with commands and they will be ran.
* `moderator` (not default): With the `command` permission the user can run any command except for `GRANT _ command/moderator`, this is so that the user can't make anyone else able to run commands. If a user has the `moderator` permission, they can run this command. Please only give this permission to people you really trust.



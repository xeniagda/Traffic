# Traffic

This is a simple traffic simulator written in `node.js` and `Elm`.

![gif of simulator running](https://media.giphy.com/media/xUPGcr428SWoTBZZ28/giphy.gif))

To run everything, run `node Server.js` in the terminal. If you are running Mac or Linux you can run the `Install.py` file to install everything necessary. If you make any changes to the Elm files, run the `Make.py` file to compile everything.

Stuff currently in the simulator:

* Roads that can be connected to each other, have speed limits etc.
* Traffic lights
* Cars with AI
* Five svg car textures (yellow, blue, police, tiger and Elm), with broken variants to (called "trasig", Swedish for "broken").
* The ability to control cars from the browser
* Building roads and traffic lights from the browser
* A system for commands and permissions
* Police cars

See [Info-Server.md](https://github.com/loovjo/Traffic/blob/master/Info-Server.md) for information about running the server.

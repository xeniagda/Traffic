# Traffic

This is a simple traffic simulator written in `node.js` and `Elm`.

![screenshot](http://i.imgur.com/FFXxFW7.png)

To run everything, run `node Server.js` in the terminal. You need to have the `ws` library installed, you can install it using `npm install --save ws` If you make modifications to the Elm files, run `elm-make Base.elm --output ../index.html` in the `Web/Elm` directory.

Stuff currently in the simulator:

* Roads that can be connected to each other, have speed limits etc.
* Traffic lights
* Cars with AI
* Four hi-quality car textures (yellow, blue, tiger and Elm), with broken variants to (called "trasig", Swedish for "broken").

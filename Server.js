let http = require("http")
let fs = require("fs")
let timers = require("timers")

halfCarHeight = 50
halfCarWidth = halfCarHeight / 2

traffic = {
    cars: [
        {name: "Car1", pos: {x:7, y:5}, rot: 0, vel: 1, steering: 40}, 
        {name: "Car2", pos: {x:15, y:15}, rot: 0, vel: 5, steering: -50}
    ],
    roads: [
        {start: {x: 7, y: 5}, end: {x: 20, y: 10}, startRoadIdx: -1, endRoadIdx: -1}
    ]
}

lastTime = Date.now()

var physics = timers.setInterval(() => {
    delta = (Date.now() - lastTime) / 1000
    lastTime = Date.now()
    traffic.cars = traffic.cars.map(car => {
        theta = car.rot / 180 * Math.PI
        rx = Math.cos(theta) * delta * car.vel
        ry = Math.sin(theta) * delta * car.vel
        new_pos = {x: car.pos.x + rx, y: car.pos.y + ry}
        car.pos = new_pos

        car.rot += car.steering * delta
        return car
    })
}, 20)

var server = http.createServer((req, res) => {
    method = req.method
    url = req.url
    if (method == "GET") {
        filePath = "Web" + url
        if (url === "/Cars") {
            res.setHeader("Content-Type", "text/json")
            res.end(JSON.stringify(traffic))
        } else if (fs.existsSync(filePath)) {
            console.log(method + " " + url)
            if (!fs.lstatSync(filePath).isFile()) {
                filePath = "Web/index.html"
            }
            content = fs.readFileSync(filePath)
            res.setHeader("Content-Type", "text/html")
            res.end(content)
        }
    }
    else
        res.end("Hello!")
})

server.listen(8000)
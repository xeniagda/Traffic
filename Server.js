let http = require("http")
let fs = require("fs")
let timers = require("timers")

halfCarHeight = 50
halfCarWidth = halfCarHeight / 2

FORWARD = 0
BACKWARD = 1

function init() {
    traffic = {
        cars: [
            {name: "Lucas", img: "Car1", pos: {x:0, y:0}, rot: 0, vel: 4, steering: 0, crashed: false,
                ai: {road_queue: [
                    {road: 0, direction: FORWARD}, 
                    {road: 1, direction: FORWARD},
                    {road: 2, direction: FORWARD}
                ]}}, 
            {name: "Felix", img: "Car2", pos: {x:2, y:20}, rot: 0, vel: 4, steering: 0, crashed: false,
                ai: {road_queue: [
                    {road: 3, direction: FORWARD}, 
                    {road: 1, direction: BACKWARD},
                    {road: 4, direction: FORWARD}
                ]}}, 
        ],
        roads: [
            {start: {x: 5, y: 5}, end: {x: 10, y: 5}, startRoadIdx: -1, endRoadIdx: 1},
            {start: {x: 10, y: 5}, end: {x: 10, y: 10}, startRoadIdx: 0, endRoadIdx: 2},
            {start: {x: 10, y: 10}, end: {x: 20, y: 10}, startRoadIdx: 1, endRoadIdx: -1},
            {start: {x: 5, y: 15}, end: {x: 10, y: 10}, startRoadIdx: -1, endRoadIdx: 1},
            {start: {x: 10, y: 5}, end: {x: 15, y: 0}, startRoadIdx: 1, endRoadIdx: -1},
        ]
    }
}

init()

lastTime = Date.now()
totalTime = 0

toRadians = (theta => theta * Math.PI / 180)
toDegrees = (theta => theta * 180 / Math.PI)

function distance(a, b) {
    dx = a.x - b.x
    dy = a.y - b.y
    return Math.sqrt(dx * dx + dy * dy)
}

var physics = timers.setInterval(() => {
    delta = (Date.now() - lastTime) / 1000
    totalTime += delta
    lastTime = Date.now()

    traffic.cars = traffic.cars.map(car => {
        theta = toRadians(car.rot)
        rx = Math.cos(theta) * delta * car.vel
        ry = Math.sin(theta) * delta * car.vel
        new_pos = {x: car.pos.x + rx, y: car.pos.y + ry}
        car.pos = new_pos

        car.rot += car.steering * delta * 5

        if (!car.crashed && traffic.cars.filter(car2 => car != car2 && distance(car.pos, car2.pos) < 1).length > 0) {
            car.img += "Trasig"
            car.crashed = true
            car.vel *= -1
        }

        // Calculate AI
        if (!car.crashed && car.ai && car.ai.road_queue.length > 0) {
            current_path = car.ai.road_queue[0]

            road = traffic.roads[current_path.road]
            road_delta = {x: road.end.x - road.start.x, y: road.end.y - road.start.y}
            road_rot = toDegrees(Math.atan2(road_delta.y, road_delta.x))

            if (road_delta.x == 0) {
                k = 1 / 0
                m = 0
                cx = road.start.x
                cy = car.pos.y
            }
            else {
                k = road_delta.y / road_delta.x
                m = road.start.y - k * road.start.x

                cx = (car.pos.x + k * car.pos.y - k * m) / (1 + k * k)
                cy = k * cx + m
            }
            closest = {x:cx, y:cy}
            if (distance(closest, road.start) + distance(closest, road.end) > distance(road.start, road.end)) {
                if (distance(closest, road.start) < distance(closest, road.end))
                    closest = road.start
                else
                    closest = road.end
            }
            dist = distance(car.pos, closest)
            dist_exag = Math.exp(3 * dist)

            // Steer car towards closest point
            wanted_end_pos = current_path.direction == FORWARD ? road.end : road.start

            wanted_rot = (toDegrees(Math.atan2(car.pos.y - (closest.y * dist_exag + wanted_end_pos.y) / (dist_exag + 1), car.pos.x - (closest.x * dist_exag + wanted_end_pos.x) / (dist_exag + 1)))) % 360 + 180

            car.steering = wanted_rot - car.rot

            car.steering = (car.steering + 180) % 360 - 180

            dwx = wanted_end_pos.x - car.pos.x
            dwy = wanted_end_pos.y - car.pos.y
            dist_to_finish = Math.sqrt(dwx * dwx + dwy * dwy)

            if (dist_to_finish < 1) {
                car.ai.road_queue.shift()
            }
        }
        else {
            car.vel /= Math.pow(5, delta)
            car.steering /= Math.pow(5, delta)
        }
        return car
    })
    if (totalTime > 20) {
        totalTime = 0
        init()
    }
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
console.log("Started")
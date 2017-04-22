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
            {name: "Gul1", img: "Car1", pos: {x:12, y:20}, rot: -90, accel: 0, speed: 4, maxSpeed: 8, steering: 0, hand_breaks: false, break_strength: 0.2, crashed: false,
                ai: {waiting: false,
                    road_queue: [
                    {road: 0, direction: FORWARD},
                    {road: 3, direction: FORWARD},
                ]}
            },
            {name: "Gul2", img: "Car1", pos: {x:12, y:18}, rot: -90, accel: 0, speed: 4, maxSpeed: 8, steering: 0, hand_breaks: false, break_strength: 0.2, crashed: false,
                ai: {waiting: false,
                    road_queue: [
                    {road: 0, direction: FORWARD},
                    {road: 5, direction: FORWARD},
                ]}
            },
            {name: "Gul3", img: "Car1", pos: {x:12, y:16}, rot: -90, accel: 0, speed: 4, maxSpeed: 8, steering: 0, hand_breaks: false, break_strength: 0.2, crashed: false,
                ai: {waiting: false,
                    road_queue: [
                    {road: 0, direction: FORWARD},
                    {road: 7, direction: FORWARD},
                ]}
            },
            {name: "Blå1", img: "Car2", pos: {x:0, y:12}, rot: -0, accel: 0, speed: 4, maxSpeed: 8, steering: 0, hand_breaks: false, break_strength: 0.2, crashed: false,
                ai: {waiting: false,
                    road_queue: [
                    {road: 2, direction: FORWARD},
                    {road: 7, direction: FORWARD},
                ]}
            },
            {name: "Blå2", img: "Car2", pos: {x:2, y:12}, rot: -0, accel: 0, speed: 4, maxSpeed: 8, steering: 0, hand_breaks: false, break_strength: 0.2, crashed: false,
                ai: {waiting: false,
                    road_queue: [
                    {road: 2, direction: FORWARD},
                    {road: 5, direction: FORWARD},
                ]}
            },
            {name: "Blå3", img: "Car2", pos: {x:4, y:12}, rot: -0, accel: 0, speed: 4, maxSpeed: 8, steering: 0, hand_breaks: false, break_strength: 0.2, crashed: false,
                ai: {waiting: false,
                    road_queue: [
                    {road: 2, direction: FORWARD},
                    {road: 1, direction: FORWARD},
                ]}
            },
            {name: "Tiger1", img: "Car3", pos: {x:10, y:0}, rot: 90, accel: 0, speed: 4, maxSpeed: 8, steering: 0, hand_breaks: false, break_strength: 0.2, crashed: false,
                ai: {waiting: false,
                    road_queue: [
                    {road: 4, direction: FORWARD},
                    {road: 1, direction: FORWARD},
                ]}
            },
            {name: "Tiger2", img: "Car3", pos: {x:10, y:2}, rot: 90, accel: 0, speed: 4, maxSpeed: 8, steering: 0, hand_breaks: false, break_strength: 0.2, crashed: false,
                ai: {waiting: false,
                    road_queue: [
                    {road: 4, direction: FORWARD},
                    {road: 3, direction: FORWARD},
                ]}
            },
            {name: "Tiger3", img: "Car3", pos: {x:10, y:4}, rot: 90, accel: 0, speed: 4, maxSpeed: 8, steering: 0, hand_breaks: false, break_strength: 0.2, crashed: false,
                ai: {waiting: false,
                    road_queue: [
                    {road: 4, direction: FORWARD},
                    {road: 7, direction: FORWARD},
                ]}
            },
            {name: "Elm1", img: "Car4", pos: {x:22, y:10}, rot: 180, accel: 0, speed: 4, maxSpeed: 8, steering: 0, hand_breaks: false, break_strength: 0.2, crashed: false,
                ai: {waiting: false,
                    road_queue: [
                    {road: 6, direction: FORWARD},
                    {road: 3, direction: FORWARD},
                ]}
            },
            {name: "Elm2", img: "Car4", pos: {x:20, y:10}, rot: 180, accel: 0, speed: 4, maxSpeed: 8, steering: 0, hand_breaks: false, break_strength: 0.2, crashed: false,
                ai: {waiting: false,
                    road_queue: [
                    {road: 6, direction: FORWARD},
                    {road: 1, direction: FORWARD},
                ]}
            },
            {name: "Elm3", img: "Car4", pos: {x:18, y:10}, rot: 180, accel: 0, speed: 4, maxSpeed: 8, steering: 0, hand_breaks: false, break_strength: 0.2, crashed: false,
                ai: {waiting: false,
                    road_queue: [
                    {road: 6, direction: FORWARD},
                    {road: 5, direction: FORWARD},
                ]}
            },
        ],
        roads: [
            {start: {x: 12, y: 22}, end: {x: 12, y: 12}, connected_to: [3,5,7], speed_rec: 5, traffic_light: {is_green: false, green_left: 0, offset: {x: 1, y: 1}}}, // Goes upwards
            {start: {x: 10, y: 12}, end: {x: 10, y: 22}, connected_to: [], speed_rec: 5},
            {start: {x: 0, y: 12}, end: {x: 10, y: 12}, connected_to: [1,5,7], speed_rec: 5, traffic_light: {is_green: false, green_left: 0, offset: {x: -1, y: 1}}}, // Goes to the right
            {start: {x: 10, y: 10}, end: {x: 0, y: 10}, connected_to: [], speed_rec: 5},
            {start: {x: 10, y: 0}, end: {x: 10, y: 10}, connected_to: [1,3,7], speed_rec: 5, traffic_light: {is_green: false, green_left: 0, offset: {x: -1, y: -1}}}, // Goes downwards
            {start: {x: 12, y: 10}, end: {x: 12, y: 0}, connected_to: [], speed_rec: 5},
            {start: {x: 22, y: 10}, end: {x: 12, y: 10}, connected_to: [1,3,5], speed_rec: 5, traffic_light: {is_green: false, green_left: 0, offset: {x: 1, y: -1}}}, // Goes to the left
            {start: {x: 12, y: 12}, end: {x: 22, y: 12}, connected_to: [], speed_rec: 5},
        ]
    }
}

DEBUG = false

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

    traffic.roads.forEach(road => {
        if (!road.traffic_light)
            return
        road.traffic_light.waiting_cars = []

        if (road.traffic_light.is_green) {
            road.traffic_light.green_left -= delta
            if (road.traffic_light.green_left <= 0) {
                road.traffic_light.green_left = 0
                road.traffic_light.is_green = false
            }
        }
    })

    traffic.cars = traffic.cars.map(car => {
        theta = toRadians(car.rot)
        rx = Math.cos(theta) * delta * car.speed
        ry = Math.sin(theta) * delta * car.speed
        new_pos = {x: car.pos.x + rx, y: car.pos.y + ry}
        car.pos = new_pos

        car.speed += car.accel * delta

        // if (car.speed > car.maxSpeed)
        //     car.speed = car.maxSpeed

        car.rot += car.steering * delta * 5

        if (!car.crashed && traffic.cars.filter(car2 => car != car2 && distance(car.pos, car2.pos) < 0.8).length > 0) {
            car.crashed = true
            car.speed *= -1
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

            // How far would the car go if the breaks were all down? Use the geometric series b+b^2+b^3... where b = break factor
            break_dist = 3 * (car.speed * car.break_strength / (1 + car.break_strength) + 1)

            // What cars are in front?
            cars_in_front = traffic.cars.filter(check => {
                if (check == car)
                    return false

                if (distance(car.pos, check.pos) > break_dist)
                    return false

                pos_delta = {x: car.pos.x - check.pos.x, y: car.pos.y - check.pos.y}
                delta_angle = toDegrees(Math.atan2(pos_delta.y, pos_delta.x))
                angle_diff = car.rot - delta_angle + 180

                while (angle_diff > 180)
                    angle_diff -= 360


                return Math.abs(angle_diff) < 15
            })

            car.hand_breaks = false

            if (road.traffic_light && dist_to_finish < 5) {
                car.ai.waiting = true
            }
            else {
                car.ai.waiting = false
            }

            // Check if there's a traffic light
            if (road.traffic_light && !road.traffic_light.is_green && dist_to_finish - 1 < break_dist) {
                car.hand_breaks = true
                car.accel = 0
            }

            else if (cars_in_front.length > 0) {
                if (DEBUG)
                    console.log(car.name + " is behind " + cars_in_front.map(car => car.name))
                // Match the speed
                avg_speed = cars_in_front.map(car => car.speed).reduce((a, b) => a + b) / cars_in_front.length
                min_dist = cars_in_front.map(check_car => distance(car.pos, check_car.pos)).reduce((a, b) => a < b ? a : b)

                if (avg_speed < car.speed - 4) {
                    car.hand_breaks = true
                }
                car.accel = (cars_in_front[0].speed - car.speed) * 3

                if (min_dist < 5) {
                    car.hand_breaks = true
                    car.accel = 0
                }

                any_waiting = cars_in_front.map(car => car.ai.waiting).reduce((a, b) => (a || b))
                car.ai.waiting |= any_waiting
            }
            else {
                car.accel = (road.speed_rec - car.speed) * 5
                if (DEBUG)
                    console.log(car.name + " is accelling from " + car.speed + " to " + road.speed_rec + " with " + car.accel)
            }
            if (road.traffic_light && car.ai.waiting) {
                road.traffic_light.waiting_cars.push(car)
            }

        }
        else {
            car.hand_breaks = true
        }

        if (car.hand_breaks) {
            car.speed *= Math.pow(car.break_strength, delta)
            car.steering /= Math.pow(5, delta)
        }

        return car
    })

    any_green = false

    max_cars = 0
    max_cars_idx = -1

    for (i = 0; i < traffic.roads.length; i++) {
        if (!traffic.roads[i].traffic_light)
            continue

        amount_of_cars = traffic.roads[i].traffic_light.waiting_cars.length
        if (traffic.roads[i].traffic_light.is_green) {
            any_green = true
            max_cars_idx = i
            break
        }

        if (amount_of_cars > max_cars) {
            max_cars = amount_of_cars
            max_cars_idx = i
        }
    }
    
    if (!any_green && max_cars_idx != -1) {
        traffic.roads[max_cars_idx].traffic_light.is_green = true
        traffic.roads[max_cars_idx].traffic_light.green_left = max_cars
    }


    if (totalTime > 20) {
        totalTime = 0
        init()
        console.log("Reset!")
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
        } else {
            console.log(method + " " + url + " (NOEXIST!)")
            res.end("NOEXIST!")
        }
    }
    else
        res.end("Hello!")
})

server.listen(8000)
console.log("Started")
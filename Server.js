let http = require("http")
let fs = require("fs")

resp = JSON.stringify({name: "Jonathan", age: 420})

console.log(resp)

var server = http.createServer((req, res) => {
    method = req.method
    url = req.url
    console.log(method + "/" + url)

    if (method == "GET") {
        filePath = "Web" + url
        if (fs.existsSync(filePath)) {
            content = fs.readFileSync(filePath)
            res.setHeader("Content-Type", "text/html")
            res.end(content)
        }
        else if (url === "/Hej") {
            console.log(resp)
            res.setHeader("Content-Type", "text/json")
            res.end(resp)
        }
    }
    else
        res.end("Hello!")
})

server.listen(8000)
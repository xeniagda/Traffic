import subprocess
import sys
import os


def command_exists(command):
    proc = subprocess.Popen(["hash", command], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    return proc.stderr.read() == b''


if not command_exists("node"):
    if len(sys.argv) < 2:
        print("Run as python3 Make.py <package manager>")
        sys.exit(1)
    PACKAGE = " ".join(sys.argv[1:])
    if not command_exists(PACKAGE.split(" ")[0]):
        print("Can't find package manager %s. Some common ones to try are brew install (mac), apt-get install (most linux distros)")
        sys.exit(1)
    print("Node not installed, install it?")
    res = input("[Y/n]")
    if res == "n":
        print("Cancelling installation")
        sys.exit(1)
    inst = subprocess.Popen((PACKAGE + " nodejs").split())
    if inst.wait():
        print("nodejs not found, trying node.js instead...")
        inst = subprocess.Popen((PACKAGE + " node.js").split())

if not command_exists("elm"):
    if len(sys.argv) < 2:
        print("Run as python3 Make.py <package manager>")
        sys.exit(1)

    PACKAGE = " ".join(sys.argv[1:])
    if not command_exists(PACKAGE.split(" ")[0]):
        print("Can't find package manager %s. Some common ones to try are brew install (mac), apt-get install (most linux distros)")
        sys.exit(1)
    print("Elm not installed, install it?")
    res = input("[Y/n]")
    if res == "n":
        print("Cancelling installation")
        sys.exit(1)
    inst = subprocess.Popen((PACKAGE + " elm").split())

for module in "ws", "ip":
    a = subprocess.Popen(["npm", "ls", module], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    errorCode = a.wait()
    if errorCode != 0:
        print("%s isn't installed. Install it?" % module)
        res = input("[Y/n]")
        if res == "n":
            print("Cancelling installation")
            sys.exit(1)
        subprocess.run(["npm", "install", module])

if not os.path.isdir("UglifyJS"):
    res = input("Install uglifyjs? (Not requiered) [y/N] ").lower()
    if res == "y":
        os.system("git clone git://github.com/mishoo/UglifyJS.git")


import os
import subprocess
import sys

print("Compiling:")
for root, dirs, files in os.walk("Source"):
    for source in files:
        if source.endswith(".elm"):
            full_path = os.path.join(root, source)
            output = os.path.join("..", "Web", "Out", source.replace(".elm", ".js"))
            print("Compiling %s -> %s" % (full_path, output))
            proc = subprocess.Popen(["elm-make", full_path, "--output", output])
            if proc.wait() != 0:
                print("Cancelling compile")
                sys.exit(1)
            if os.path.exists("../UglifyJS/bin/uglifyjs"):
                print("Uglifying %s" % output)
                proc = subprocess.Popen(["../UglifyJS/bin/uglifyjs", "--output", output, output])
                if proc.wait() != 0:
                    print("Cancelling compile")
                    sys.exit(1)
                print("Done!")


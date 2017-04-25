import os
import subprocess

print("Compiling:")
for root, dirs, files in os.walk("Source"):
    for source in files:
        if source.endswith(".elm"):
            full_path = os.path.join(root, source)
            output = os.path.join("..", "Web", "Out", source.replace(".elm", ".js"))
            print("Compiling", full_path, "->", output)
            subprocess.run(["elm-make", full_path, "--output", output])
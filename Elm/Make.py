import os
import subprocess
import sys
import time

if "-h" in sys.argv[1:] or "--help" in sys.argv[1:]:
    from textwrap import dedent
    
    print(dedent("""\
        Usage: python3 %s [-a] [-h]
        
        %s is a program to compile this project. It compiles and minifies every file in the Source/ directory. 

        Options:
            -a      Only compiles modified files by checking the file hashes from the last compile
            -h      Show this help message
            -H      Set the file to read the hashes from. Default is .hashes
            -M      Don't minify the output, makes the compile faster but less efficient
        """ % (__file__, __file__)))
    sys.exit()

FILES_TO_COMPILE = ["Source/Main.elm"]

CHECK_HASH = "-a" in sys.argv[1:]
HASH_PATH = ".hashes"

if "-H" in sys.argv[1:] and sys.argv[-1] != "-H":
    idx = sys.argv.index("-H")
    HASH_PATH = sys.argv[idx + 1]

hashes = {}

import hashlib
from ast import literal_eval

if os.path.isfile(HASH_PATH):
    try:
        hashes = literal_eval(open(HASH_PATH, "r").read())
    except:
        print("Invalid .hashes file!")
        remake = input("Reinitialize the file? [Y/n] ").lower()
        if remake != "n":
            open(HASH_PATH, "w").close()
else:
    open(HASH_PATH, "w").close()

print("Starting compile...")

compiled = 0
start = time.time()

for root, dirs, files in os.walk("Source"):
    for source in files:
        if source.endswith(".elm"):
            full_path = os.path.join(root, source)
            if not full_path in FILES_TO_COMPILE:
                continue
            print()
            changed = True

            content = open(full_path, "r").read()
            file_hash = hashlib.sha256(content.encode("ascii")).hexdigest()

            if full_path in hashes:
                if hashes[full_path] == file_hash:
                    changed = False
            hashes[full_path] = file_hash

            output = os.path.join("..", "Web", "Out", source.replace(".elm", ".js"))
            print("Compiling %s -> %s" % (full_path, output))

            if not changed and CHECK_HASH:
                print("File not changed since last compile, skipping this file. (Use -a/--all to compile anyway)")
                continue

            proc = subprocess.Popen(["elm-make", full_path, "--output", output])
            if proc.wait() != 0:
                print("Cancelling compile")
                sys.exit(1)

            if os.path.exists("../UglifyJS/bin/uglifyjs") and not "-M" in sys.argv[1:]:
                print("Minifying %s" % output)
                proc = subprocess.Popen(["../UglifyJS/bin/uglifyjs", "--output", output, output])
                if proc.wait() != 0:
                    print("Cancelling compile")
                    sys.exit(1)
            print("Done compiling %s" % full_path)
            compiled += 1

compile_time = time.time() - start

print("\nCompiled %i files total, took %.2f seconds" % (compiled, compile_time))

hash_file = open(HASH_PATH, "w")
hash_file.write(repr(hashes))

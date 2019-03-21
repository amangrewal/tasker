#!/usr/bin/env python
import sys
import os.path

if os.path.exists("/tmp/tasks-input"):
    with open("/tmp/tasks-input", "w") as f:
        f.write("\x00".join(sys.argv[1:])+"\n")
    #This will error if the file doesn't exist
    with open("/tmp/tasks-output", "r") as f:
        print(f.read())

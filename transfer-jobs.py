#!/usr/bin/python

import subprocess

jobNames = ["runew-34", "runew-35"]

for job in jobNames:
    retcode = subprocess.call(["rsync", "-ahvi", "--append",
                               "scylla:~/scratch/" + job,
                               "/home/poison/deepak/ROMS/runs/eddyshelf/topoeddy/"])

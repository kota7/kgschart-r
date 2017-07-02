#!/usr/bin/env python
# -*- coding: utf-8 -*- 

"""
Rename image files as sample-NN_NN.png,
where first NN is the folder name, and 
the second the file id

assumptions:
  - image folders are named as numbers like 1, 2, 3... 

usage:  
  - move to the directory where this script exists
  - run the script from the console 
  - when prompted, type the target directory name 
"""



import os
import shutil
import tempfile
import sys
from glob import glob

def main():
    root = os.path.dirname(__file__)
    d = input(" type target directory name> ")
    file_dir = os.path.join(root, d)


    # is this a directory?
    if not os.path.isdir(file_dir):
        print(d, "is not a directory")
        sys.exit(1)

    # check if the file names are already "sample..."
    tmp = [os.path.basename(fn) for fn in glob(os.path.join(file_dir, "sample*.png"))]
    if len(tmp) > 2:
        ans = input(" are you sure? the files seems to have already renamed y/[n] >")
        if ans == "" or ans[0] != "y":
            print(" yup! see you!")
            return

    # move files to temp directory
    # because direct renaming may lose existing files if the names coincide
    tempdir = tempfile.mkdtemp()
    i = 0
    for fn in glob(os.path.join(file_dir, "*.png")): 
        i += 1
        shutil.move(fn, os.path.join(tempdir, os.path.basename(fn)))

    i = 0
    for fn in glob(os.path.join(tempdir, "*.png")): 
        i += 1
        newname = os.path.join(d, \
                "sample-" + d.zfill(2) + "_" + str(i).zfill(2) + ".png")
        print("%-30s -> %s" % (os.path.basename(fn), newname))
        shutil.move(fn, newname)

main()




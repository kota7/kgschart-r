#!/usr/bin/env python
# -*- coding: utf-8 -*-


import os
import shutil
import sys
from glob import glob

def main():
    """
    copy all images and json files
    """
    
    root = os.path.dirname(os.path.abspath(__file__))
    d = input("choose directory to copy > ").replace("/", "")
    file_dir = os.path.join(root, d)
    if not os.path.isdir(file_dir):
        print(file_dir, "is not a directory")
        sys.exit(1)

    ansfile = os.path.join(root, 'ans-%s.json' % d)
    dest = os.path.abspath(os.path.join(root, "../"))

    print("copying from:")
    print(" directory  :", file_dir)
    print(" json file  :", ansfile)
    print(" destination:", dest)
    ans = input("okay? y/[n]")
    if len(ans) == 0 or ans[0] != "y":
        print("see you!")
        return

    # copy image files
    for fn in glob(os.path.join(file_dir, "*.png")):
        print("copying:", os.path.basename(fn))
        shutil.copyfile(fn, os.path.join(dest, os.path.basename(fn)))
   
    # copy answer file
    print("copying:", os.path.basename(ansfile))
    shutil.copyfile(ansfile, os.path.join(dest, os.path.basename(ansfile)))
main()

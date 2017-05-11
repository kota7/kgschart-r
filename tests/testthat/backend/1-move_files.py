#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
from glob import glob
from shutil import copyfile

if __name__ == '__main__':
    d = os.path.dirname(__file__)    
    savedir = os.path.abspath(os.path.join(d, '../'))
    print('files will be saved in:', savedir)
    
    count = 0
    for fn in glob('*/*.png'):
        count += 1
        savename = 'sample-%02d.png' % count 
        print(fn, '\t-->', savename)
        copyfile(fn, os.path.join(savedir, savename))
    print('Done') 





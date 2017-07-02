#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import sys
from glob import glob
import json
from PIL import Image
import numpy as np
from matplotlib import pyplot as plt


def main():
    d = os.path.dirname(__file__)
    
    file_dir = input(" type directory name > ")
    if not os.path.isdir(file_dir):
        print(file_dir, "is not a directory")
        sys.exit(1)

    answer_file = 'ans-%s.json' % file_dir 
    files_dir = os.path.join(d, file_dir)
    
    
    plt.plot(1)
    plt.show(block=False)
    out = []
    for fn in glob(os.path.join(file_dir, '*.png')):  
        x = np.asarray(Image.open(fn))
        plt.imshow(x)
        plt.draw()

        tmp = {'file':os.path.basename(fn)}
        while True:
            print('-------------------')
            print('file:', os.path.basename(fn))
            print('* rank is like "10k", "5d"')
            print('* date is like in "yyyy-mm-dd" or "yyyy-mm-dd HH:MM"')
            print('* type "None" if missing')

            ans1 = input('* min rank : ')
            ans2 = input('* max rank : ')
            ans3 = input('* start date (time) : ')
            ans4 = input('* end date (time)   : ')

            tmp['rank_range'] = (ans1, ans2)
            tmp['time_range'] = (ans3, ans4)
            
            while True:
                ans = input('* okay? (y or n) > ') 
                if ans in ['y', 'n']: 
                    break
            if ans == 'y':
                out.append(tmp)
                break

    with open(answer_file, 'w') as f:
        json.dump(out, f, indent=2)

main()



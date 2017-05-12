#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
from glob import glob
import json
from PIL import Image
import numpy as np
from matplotlib import pyplot as plt

if __name__ == '__main__':
    d = os.path.dirname(__file__)
    
    # change here #############
    file_dir = '3'
    answer_file = 'ans-3.json'
    ###########################
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

            ans1 = input('* min rank is: ')
            ans2 = input('* max rank is: ')
            ans3 = input('* start date (time) is: ')
            ans4 = input('* end date (time) is  : ')

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





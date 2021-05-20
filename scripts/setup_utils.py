#!/usr/bin/env python3

# Copyright (C) 2021 Robert Coffey
# Released under the GPLv2 license

import glob

setup_path = '../res/setups/'

def sort_by_title_number(element):
    return int(element.split('-')[1].split('.')[0])

def open_new_setup_file():
    random_setup_files = sorted(glob.glob(setup_path + 'random-*.csv'), key=sort_by_title_number)

    if len(random_setup_files) == 0:
        next_number = 0
    else:
        next_number = int(random_setup_files[-1].split('-')[1].split('.')[0]) + 1

    print(f'Creating {setup_path}random-{next_number}.csv')
    return open(setup_path + 'random-' + str(next_number) + '.csv', 'w')

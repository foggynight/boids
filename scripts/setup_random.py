#!/usr/bin/env python3

# Copyright (C) 2021 Robert Coffey
# Released under the GPLv2 license

from random import randrange

from setup_utils import *

def gen_random_boid():
    x = randrange(SCREEN_WIDTH)
    y = randrange(SCREEN_HEIGHT)
    angle = randrange(MAX_ANGLE)
    velocity = DEFAULT_VELOCITY
    return f'{x}.0,{y}.0,{angle}.0,{velocity}.0\n'

if __name__ == '__main__':
    random_setup_files = sorted(glob.glob(SETUP_PATH + 'random-*.csv'), key=sort_by_title_number)

    if len(random_setup_files) == 0:
        next_number = 0
    else:
        next_number = int(random_setup_files[-1].split('-')[1].split('.')[0]) + 1

    setup_file = setup_open_new(f'random-{next_number}')

    for i in range(BOID_COUNT):
        setup_file.write(gen_random_boid())

    setup_file.close()

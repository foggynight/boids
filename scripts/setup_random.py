#!/usr/bin/env python3

# Copyright (C) 2021 Robert Coffey
# Released under the GPLv2 license

from glob import glob
from random import randrange

from setup_utils import *

def gen_random_boid():
    x = randrange(SCREEN_WIDTH)
    y = randrange(SCREEN_HEIGHT)
    angle = randrange(BOID_MAX_ANGLE)
    velocity = BOID_DEFAULT_VELOCITY
    return f'{x},{y},{angle},{velocity}\n'

if __name__ == '__main__':
    random_files = glob(SETUP_PATH + '/' + 'random-*.csv')
    random_files.sort(key=get_title_number)

    if len(random_files) == 0:
        next_number = 0
    else:
        next_number = int(random_files[-1].split('-')[1].split('.')[0]) + 1

    file = setup_open_new(f'random-{next_number}')

    for i in range(BOID_COUNT):
        file.write(gen_random_boid())

    file.close()

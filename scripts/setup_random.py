#!/usr/bin/env python3

# Copyright (C) 2021 Robert Coffey
# Released under the GPLv2 license

from random import randrange

from setup_utils import *

SCREEN_WIDTH = 1280
SCREEN_HEIGHT = 720

def gen_random_boid():
    x = randrange(SCREEN_WIDTH)
    y = randrange(SCREEN_HEIGHT)
    angle = randrange(360)
    velocity = 1
    return f'{x}.0,{y}.0,{angle}.0,{velocity}.0\n'

if __name__ == '__main__':
    number_of_boids = 16
    setup_file = open_new_setup_file()

    for i in range(number_of_boids):
        setup_file.write(gen_random_boid())

    setup_file.close()

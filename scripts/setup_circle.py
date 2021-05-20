#!/usr/bin/env python3

# Copyright (C) 2021 Robert Coffey
# Licensed under the GPLv2 license

from math import cos, degrees, sin

from setup_utils import *

if __name__ == '__main__':
    file = setup_open_new('circle')

    scr_center_x = SCREEN_WIDTH / 2
    scr_center_y = SCREEN_HEIGHT / 2

    angle = 0
    angle_step = MAX_ANGLE / BOID_COUNT
    radius = 100

    for i in range(BOID_COUNT):
        x = int(scr_center_x + radius * cos(degrees(angle)))
        y = int(scr_center_y + radius * sin(degrees(angle)))
        angle += angle_step
        file.write(f'{x}.0,{y}.0,{angle}.0,{DEFAULT_VELOCITY}.0\n')

    file.close()

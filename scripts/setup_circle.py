#!/usr/bin/env python3

# Copyright (C) 2021 Robert Coffey
# Licensed under the GPLv2 license

from math import cos, radians, sin

from setup_utils import *

if __name__ == '__main__':
    file = setup_open_new('circle')

    scr_center_x = SCREEN_WIDTH / 2
    scr_center_y = SCREEN_HEIGHT / 2

    angle = 0
    angle_step = BOID_MAX_ANGLE / BOID_COUNT
    radius = 100

    for i in range(BOID_COUNT):
        x = int(scr_center_x + radius * cos(radians(angle)))
        y = int(scr_center_y + radius * sin(radians(angle)))
        file.write(f'{x},{y},{angle},{BOID_DEFAULT_VELOCITY}\n')
        angle += angle_step

    file.close()

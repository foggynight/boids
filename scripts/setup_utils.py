#!/usr/bin/env python3

# Copyright (C) 2021 Robert Coffey
# Released under the GPLv2 license

import os

BOID_COUNT = 16

BOID_W = 32
BOID_H = 32

BOID_MAX_ANGLE = 360
BOID_DEFAULT_VELOCITY = 100

SCREEN_WIDTH = 1280
SCREEN_HEIGHT = 720

SETUP_PATH = '../res/setups'

def get_title_number(element):
    return int(element.split('-')[1].split('.')[0])

def setup_open_new(setup_name):
    if os.getcwd().split('/')[-1] != 'scripts':
        exit("Invalid usage: Working directory must be '.../boids/scripts'")

    print(f'Creating {SETUP_PATH}/{setup_name}.csv')

    return open(SETUP_PATH + '/' + setup_name + '.csv', 'w')

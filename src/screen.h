// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#ifndef SCREEN_H
#define SCREEN_H

#include "boid.h"

#define WIN_WIDTH	1280
#define WIN_HEIGHT	720

extern int boid_sprite_w, boid_sprite_h;

void screen_init(void);
void screen_destroy(void);
void screen_update(boid_t boids[], size_t boid_count);

#endif	// SCREEN_H

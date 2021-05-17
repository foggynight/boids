// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <SDL2/SDL.h>

#include "boid.h"
#include "screen.h"

#define MAX_BOID_COUNT	16

#define TARGET_FPS	144
#define SLEEP_TIME	(1000 / TARGET_FPS)

int main(void)
{
	SDL_Event event;

	boid_t boids[MAX_BOID_COUNT] = {0};
	size_t boid_count = 0;

	boids[0].x = boids[0].y = 0.0f;
	boids[0].angle = 45;
	boids[0].velocity = 1.0f;
	boid_count = 1;

	screen_init();
	while (1) {
		SDL_PollEvent(&event);
		if (event.type == SDL_QUIT)
			break;

		boid_update(boids, boid_count);
		screen_update(boids, boid_count);

		SDL_Delay(SLEEP_TIME);
	}
	screen_destroy();

	return 0;
}

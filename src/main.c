// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <SDL.h>

#include "screen.h"

#define MAX_BOID_COUNT	16

#define TARGET_FPS	144
#define SLEEP_TIME	(1000 / TARGET_FPS)

int main(void)
{
	SDL_Event event;

	size_t boid_count = 0;
	boid_t boids[MAX_BOID_COUNT] = {0};

	screen_init();
	while (1) {
		SDL_PollEvent(&event);
		if (event.type == SDL_QUIT)
			break;

		screen_update(boids, boid_count);

		SDL_Delay(SLEEP_TIME);
	}
	screen_destroy();

	return 0;
}

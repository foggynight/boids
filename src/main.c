// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <SDL.h>

#include "screen.h"

#define TARGET_FPS	144
#define SLEEP_TIME	(1000 / TARGET_FPS)

int main(void)
{
	SDL_Event event;

	screen_init();

	while (1) {
		SDL_PollEvent(&event);
		if (event.type == SDL_QUIT)
			break;

		screen_update();

		SDL_Delay(SLEEP_TIME);
	}

	screen_destroy();

	return 0;
}

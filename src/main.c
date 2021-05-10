// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <SDL.h>

#include "screen.h"

#define TARGET_FPS	144
#define SLEEP_TIME	(1000 / TARGET_FPS)

int main(void)
{
	screen_init();

	while (screen_update())
		SDL_Delay(SLEEP_TIME);

	screen_destroy();

	return 0;
}

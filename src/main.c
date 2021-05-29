// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <stdio.h>
#include <string.h>
#include <time.h>

#include <SDL2/SDL.h>

#include "boid.h"
#include "screen.h"
#include "setup.h"

#define SETUP_FILE_PATH	"./res/setups/"

#define FRAMES_PER_SECOND	144
#define CLOCKS_PER_FRAME	(CLOCKS_PER_SEC / FRAMES_PER_SECOND)

int main(int argc, char **argv)
{
	char setup_file_fullpath[FILENAME_MAX+1] = SETUP_FILE_PATH;
	if (argc > 2) {
		printf("Invalid usage: Too many arguments\n");
		return 1;
	}
	else if (argc < 2) {
		strcat(setup_file_fullpath, "default");
	}
	else {
		strcat(setup_file_fullpath, argv[1]);
	}
	strcat(setup_file_fullpath, ".csv");

	FILE *setup_file = fopen(setup_file_fullpath, "r");
	if (!setup_file) {
		printf("Error: Failed to open setup file: %s\n", setup_file_fullpath);
		return 1;
	}

	setup_boid_arr(setup_file);

	clock_t time = 0;
	clock_t time_delta = 0;

	screen_init();
	while (1) {
		time = clock();

		SDL_Event event;
		SDL_PollEvent(&event);
		if (event.type == SDL_QUIT)
			break;

		screen_update();
		boid_update(time_delta);

		do {
			time_delta = clock() - time;
		} while (time_delta < CLOCKS_PER_FRAME);
	}
	screen_destroy();

	return 0;
}

// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <stdio.h>
#include <string.h>

#include <SDL2/SDL.h>

#include "boid.h"
#include "screen.h"
#include "setup.h"

#define SETUP_FILE_PATH	"./res/setups/"

#define MAX_BOID_COUNT	16

#define TARGET_FPS	144
#define SLEEP_TIME	(1000 / TARGET_FPS)

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

	boid_t boids[MAX_BOID_COUNT] = {0};
	size_t boid_count = setup_boids(setup_file, boids, MAX_BOID_COUNT);

	screen_init();
	while (1) {
		SDL_Event event;
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

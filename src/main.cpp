// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <chrono>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <vector>

#include <SDL2/SDL.h>

#include "Boid.hpp"
#include "screen.hpp"
#include "setup.hpp"

#define SETUP_FILE_PATH	"./res/setups/"

#define FRAMES_PER_SECOND	144
#define MICROSECONDS_PER_SECOND	1000000
#define MICROSECONDS_PER_FRAME	((float)MICROSECONDS_PER_SECOND / (float)FRAMES_PER_SECOND)

int main(int argc, char **argv)
{
	char setup_file_fullpath[FILENAME_MAX+1] = SETUP_FILE_PATH;
	if (argc > 2) {
		std::cout << "Invalid usage: Too many arguments" << std::endl;
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
		std::cout << "Error: Failed to open setup file: " << setup_file_fullpath << std::endl;
		return 1;
	}

	std::vector<Boid> boid_vec;
	setup_boid_vec(boid_vec, setup_file);

	auto last_time = std::chrono::high_resolution_clock::now();

	screen_init();
	while (1) {
		auto delta_time = std::chrono::high_resolution_clock::now() - last_time;
		last_time = std::chrono::high_resolution_clock::now();
		int delta_time_us = std::chrono::duration_cast<std::chrono::microseconds>(delta_time).count();

		std::cout << "\rFPS: " << MICROSECONDS_PER_SECOND / delta_time_us;
		std::flush(std::cout);

		SDL_Event event;
		SDL_PollEvent(&event);
		if (event.type == SDL_QUIT)
			break;

		screen_update(boid_vec);
		for (auto &boid : boid_vec) {
			boid.update_pos(delta_time_us);
		}

		while (std::chrono::duration_cast<std::chrono::microseconds>(std::chrono::high_resolution_clock::now() - last_time).count() < MICROSECONDS_PER_FRAME);
	}
	screen_destroy();

	std::cout << std::endl;	// Print newline after FPS counter

	return 0;
}

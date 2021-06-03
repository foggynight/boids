// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <chrono>
#include <iostream>
#include <string>
#include <vector>

#include <SDL2/SDL.h>

#include "Boid.hpp"
#include "screen.hpp"
#include "setup.hpp"

extern "C" {
#include "util.h"
}

#define SETUP_FILE_PATH	"./res/setups/"

#define FPS	144
#define US_PER_FRAME	((float)US_PER_SECOND / (float)FPS)

int main(int argc, char **argv)
{
	std::string setup_file_fullpath = SETUP_FILE_PATH;
	if (argc > 2) {
		std::cout << "Invalid usage: Too many arguments" << std::endl;
		return 1;
	}
	else if (argc < 2)
		setup_file_fullpath += "default";
	else
		setup_file_fullpath += argv[1];
	setup_file_fullpath += ".csv";

	std::vector<Boid> boid_vec;
	if (!setup_boid_vec(boid_vec, setup_file_fullpath)) {
		std::cout << "Error: Failed to open setup file: " << setup_file_fullpath << std::endl;
		return 1;
	}

	auto last_time = std::chrono::high_resolution_clock::now();

	screen::init();
	while (1) {
		const auto delta_time = std::chrono::high_resolution_clock::now() - last_time;
		last_time = std::chrono::high_resolution_clock::now();

		const int delta_time_us = std::chrono::duration_cast<std::chrono::microseconds>(delta_time).count();

		//std::cout << "\rFPS: " << (float)US_PER_SECOND / (float)delta_time_us;
		//std::flush(std::cout);

		SDL_Event event;
		SDL_PollEvent(&event);
		if (event.type == SDL_QUIT)
			break;

		screen::clear();
		screen::draw_boids(boid_vec);

		std::vector<Boid *> neighbor_vec;
		for (auto& boid : boid_vec) {
			neighbor_vec.clear();
			boid.get_neighbors(boid_vec, neighbor_vec);

			screen::draw_fov(boid);
			for (auto& neighbor : neighbor_vec)
				screen::draw_line_between(boid, *neighbor);

			if (neighbor_vec.size() > 0) {
				boid.align_with_neighbors(neighbor_vec, delta_time_us);
			}

			boid.update_pos(delta_time_us);
		}

		screen::present();

		for (int wait_time = 0; wait_time < US_PER_FRAME;) {
			const auto now = std::chrono::high_resolution_clock::now();
			wait_time = std::chrono::duration_cast<std::chrono::microseconds>(now - last_time).count();
		}
	}
	screen::destroy();

	std::cout << std::endl;	// Print newline after FPS counter

	return 0;
}

// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <cstdlib>
#include <vector>

#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>

#include "Boid.hpp"
#include "screen.hpp"

#define BOID_SPRITE_PATH	"res/sprites/boid_wireframe.png"	// Path to the boid sprite relative to the program executable

static SDL_Window *window;
static SDL_Renderer *renderer;

static SDL_Texture *boid_sprite_texture;

void screen_init(void)
{
	if (SDL_Init(SDL_INIT_VIDEO) != 0) {
		SDL_Log("Error: Failed to initialize SDL: %s", SDL_GetError());
		exit(1);
	}

	if ((IMG_Init(IMG_INIT_PNG) & IMG_INIT_PNG) != IMG_INIT_PNG) {
		SDL_Log("Error: Failed to initialize SDL_image: %s", IMG_GetError());
		exit(1);
	}

	window = SDL_CreateWindow(
			"hop",
			SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
			WIN_WIDTH, WIN_HEIGHT,
			0);
	if (!window) {
		SDL_Log("Error: Failed to create window: %s", SDL_GetError());
		exit(1);
	}

	renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
	if (!renderer) {
		SDL_Log("Error: Failed to create renderer: %s", SDL_GetError());
		exit(1);
	}

	boid_sprite_texture = IMG_LoadTexture(renderer, BOID_SPRITE_PATH);
	if (!boid_sprite_texture) {
		SDL_Log("Error: Failed to load boid sprite: %s", IMG_GetError());
		exit(1);
	}
}

void screen_destroy(void)
{
	SDL_DestroyTexture(boid_sprite_texture);

	SDL_DestroyRenderer(renderer);
	SDL_DestroyWindow(window);

	IMG_Quit();
	SDL_Quit();
}

void screen_update(std::vector<Boid> &boid_vec)
{
	SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
	SDL_RenderClear(renderer);

	SDL_Rect rect;
	for (auto boid : boid_vec) {
		rect.w = (int)boid.w;
		rect.h = (int)boid.h;
		rect.x = (int)(boid.x - boid.w / 2.0f);
		rect.y = (int)(boid.y - boid.h / 2.0f);

		SDL_RenderCopyEx(
				renderer,
				boid_sprite_texture,
				NULL,
				&rect,
				(int)boid.angle + 90,	// Addition of 90 is to compensate for sprites facing upwards
				NULL,
				SDL_FLIP_NONE
			);
	}

	SDL_RenderPresent(renderer);
}

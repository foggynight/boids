// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <stdlib.h>

#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>

#include "boid.h"
#include "screen.h"

#define BOID_SPRITE_PATH	"res/sprites/boid_wireframe.png"

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

void screen_update(boid_t boids[], size_t boid_count)
{
	SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
	SDL_RenderClear(renderer);

	SDL_Rect boid_sprite_rect;
	for (size_t i = 0; i < boid_count; ++i) {
		boid_sprite_rect.x = boids[i].x - boid_w/2;
		boid_sprite_rect.y = boids[i].y - boid_h/2;
		boid_sprite_rect.w = boid_w;
		boid_sprite_rect.h = boid_h;

		SDL_RenderCopyEx(
				renderer,
				boid_sprite_texture,
				NULL,
				&boid_sprite_rect,
				boids[i].angle + 90,	// Addition of 90 is to compensate for sprites facing upwards
				NULL,
				SDL_FLIP_NONE
			);
	}

	SDL_RenderPresent(renderer);
}

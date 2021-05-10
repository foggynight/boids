// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <stdlib.h>

#include <SDL.h>

#include "screen.h"

static SDL_Window *window;
static SDL_Renderer *renderer;

void screen_init(void)
{
	if (SDL_Init(SDL_INIT_VIDEO) != 0) {
		SDL_Log("Error: Failed to initialize SDL: %s", SDL_GetError());
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
}

void screen_destroy(void)
{
	SDL_DestroyRenderer(renderer);
	SDL_DestroyWindow(window);

	SDL_Quit();
}

int screen_update(void)
{
	SDL_Event event;

	SDL_PollEvent(&event);
	if (event.type == SDL_QUIT)
		return 0;

	SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
	SDL_RenderClear(renderer);

	SDL_RenderPresent(renderer);

	return 1;
}

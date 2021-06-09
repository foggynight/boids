// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <cmath>
#include <cstdlib>
#include <vector>

#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>

#include "Boid.hpp"
#include "Entity.hpp"
#include "Vec2.hpp"
#include "screen.hpp"

extern "C" {
#include "util.h"
}

// Path to the boid sprite relative to the program executable
#define BOID_SPRITE_PATH	"res/sprites/boid_wireframe.png"

static SDL_Window *window;
static SDL_Renderer *renderer;

static SDL_Texture *boid_sprite_texture;

/**
 * Draw a circular arc, which is a segment of a circle.
 *
 * @param center_x	x position at the center of the circle
 * @param center_y	y position at the center of the circle
 * @param radius	Radius of the circle
 * @param start_angle	Angle to start the arc at
 * @param end_angle	Angle to end the arc at
 **/
static void draw_circular_arc(
		float center_x, float center_y,
		float radius,
		float start_angle, float end_angle);

void screen::init()
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

void screen::destroy()
{
	SDL_DestroyTexture(boid_sprite_texture);

	SDL_DestroyRenderer(renderer);
	SDL_DestroyWindow(window);

	IMG_Quit();
	SDL_Quit();
}

void screen::clear()
{
	SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
	SDL_RenderClear(renderer);
}

void screen::present()
{
	SDL_RenderPresent(renderer);
}

void screen::draw_boids(std::vector<Boid>& boid_vec)
{
	const int w = Entity::get_width();
	const int h = Entity::get_height();

	SDL_Rect rect;
	rect.w = w;
	rect.h = h;

	for (auto& boid : boid_vec) {
		// @TODO Round these values instead of truncate
		rect.x = (int)(boid.pos.x - (float)w / 2.0f);
		rect.y = (int)(boid.pos.y - (float)h / 2.0f);

		SDL_RenderCopyEx(
				renderer,
				boid_sprite_texture,
				NULL,
				&rect,
				// @TODO Round this value instead of truncate
				(int)boid.angle + 90,	// Addition of 90 is to compensate for sprites facing upwards
				NULL,
				SDL_FLIP_NONE
			);
	}
}

void screen::draw_fov(Entity& entity)
{
	const float x_start = entity.pos.x, y_start = entity.pos.y;
	const float fov_radius = Entity::get_fov_radius();
	const float fov_max_angle = Entity::get_fov_max_angle();

	const float forward_angle = entity.angle;
	const float center_x_end = x_start + fov_radius * (float)cos(deg_to_rad(forward_angle));
	const float center_y_end = y_start + fov_radius * (float)sin(deg_to_rad(forward_angle));

	float left_angle = forward_angle - fov_max_angle;
	if (left_angle < 0)
		left_angle += 360.0f;
	const float left_x_end = x_start + fov_radius * (float)cos(deg_to_rad(left_angle));
	const float left_y_end = y_start + fov_radius * (float)sin(deg_to_rad(left_angle));

	float right_angle = forward_angle + fov_max_angle;
	if (right_angle >= 360.0f)
		right_angle -= 360.0f;
	const float right_x_end = x_start + fov_radius * (float)cos(deg_to_rad(right_angle));
	const float right_y_end = y_start + fov_radius * (float)sin(deg_to_rad(right_angle));

	SDL_SetRenderDrawColor(renderer, 100, 0, 100, 255);
	SDL_RenderDrawLine(renderer,
			x_start, y_start,
			center_x_end, center_y_end);
	SDL_RenderDrawLine(renderer,
			x_start, y_start,
			left_x_end, left_y_end);
	SDL_RenderDrawLine(renderer,
			x_start, y_start,
			right_x_end, right_y_end);

	draw_circular_arc(x_start, y_start, fov_radius, left_angle, right_angle);
}

void screen::draw_line_between(const Entity& reference, const Entity& target)
{
	SDL_SetRenderDrawColor(renderer, 0, 255, 0, 255);
	SDL_RenderDrawLine(renderer,
			reference.pos.x, reference.pos.y,
			target.pos.x, target.pos.y);
}

static void draw_circular_arc(
		float center_x, float center_y,
		float radius,
		float start_angle, float end_angle)
{
	const int steps = 2.0f * radius * (float)M_PI;
	float delta_angle = end_angle - start_angle;
	if (delta_angle < 0.0f)
		delta_angle += 360.0f;
	const float step_size = delta_angle / (float)steps;

	float angle = start_angle;
	SDL_SetRenderDrawColor(renderer, 100, 0, 100, 255);
	for (int i = 0; i < steps; ++i) {
		SDL_RenderDrawPoint(renderer,
				center_x + radius * (float)cos(deg_to_rad(angle)),
				center_y + radius * (float)sin(deg_to_rad(angle)));
		angle += step_size;
	}
}

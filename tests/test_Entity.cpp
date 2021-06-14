// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <cassert>
#include <cmath>
#include <iostream>

#include "Entity.hpp"
#include "Vec2.hpp"

extern "C" {
#include "approx_equal.h"
#include "util.h"
}

void test_constructor();
void test_rotate();
void test_rotate_towards();
void test_update_pos();
void test_in_fov();

int main()
{
	test_constructor();
	test_rotate();
	test_rotate_towards();
	test_update_pos();
	test_in_fov();
	std::cout << "test_Entity: All tests passed" << std::endl;
	return 0;
}

void test_constructor()
{
	const float start_x = 0.0f;
	const float start_y = 1.0f;
	const float start_angle = 2.0f;
	const float start_velocity = 3.0f;

	Entity entity(Vec2(start_x, start_y), start_angle, start_velocity);

	assert(entity.pos.x == start_x);
	assert(entity.pos.y == start_y);
	assert(entity.angle == start_angle);
	assert(entity.velocity == start_velocity);
}

void test_rotate()
{
	Entity entity(Vec2(0.0f, 0.0f), 0.0f, 0.0f);
	auto test = [&entity](float start_angle, float delta_angle, float new_angle) {
		entity.angle = start_angle;
		entity.rotate(delta_angle);
		assert(approx_equal(entity.angle, new_angle));
	};

	test(0, 0, 0);
	test(0, 90, 90);
	test(0, -90, 270);
	test(270, 90, 0);
	test(0, 450, 90);
	test(0, -450, 270);
}

void test_rotate_towards()
{
	// @TODO Figure out how to test this
}

void test_update_pos()
{
	Entity entity(Vec2(0.0f, 0.0f), 0.0f, 0.0f);
	auto test = [&entity](
			float x, float y,
			float angle, float velocity,
			float new_x, float new_y)
	{
		entity.pos.x = x;
		entity.pos.y = y;
		entity.angle = angle;
		entity.velocity = velocity;
		entity.update_pos(1.0f * US_PER_SECOND);
		assert(approx_equal(entity.pos.x, new_x));
		assert(approx_equal(entity.pos.y, new_y));
	};

	test(0, 0, 0, 0, 0, 0);
	test(0, 0, 0, 1, 1, 0);
	test(0, 0, 45, sqrt(2), 1, 1);
	test(1, 0, 180, 1, 0, 0);
	test(1, 0, 135, sqrt(2), 0, 1);
	test(0, 0, 90, 1, 0, 1);
	test(1, 1, 225, sqrt(2), 0, 0);
	test(0, 1, 270, 1, 0, 0);
	test(0, 1, 315, sqrt(2), 1, 0);
}

void test_in_fov()
{
	const float initial_fov_radius = Entity::get_fov_radius();
	const float initial_fov_max_angle = Entity::get_fov_max_angle();

	const float fov_radius = 100.0f;
	const float fov_max_angle = 120.0f;

	Entity::set_fov_radius(fov_radius);
	Entity::set_fov_max_angle(fov_max_angle);

	Entity entity0(Vec2(0.0f, 0.0f), 0.0f, 0.0f);	// Looker
	Entity entity1(Vec2(0.0f, 0.0f), 0.0f, 0.0f);	// Target
	auto test = [&entity0, &entity1](
			float x0, float y0, float angle0,
			float x1, float y1,
			bool is_in_fov)
	{
		entity0.pos.x = x0;
		entity0.pos.y = y0;
		entity0.angle = angle0;
		entity1.pos.x = x1;
		entity1.pos.y = y1;
		assert(entity0.in_fov(entity1) == is_in_fov);
	};

	test(0, 0, 0, 0, 0, true);
	test(0, 0, 180, 0, 0, false);
	test(0, 0, 0, fov_radius-1, 0, true);
	test(0, 0, 0, fov_radius, 0, true);
	test(0, 0, 0, fov_radius+1, 0, false);
	test(0, 0, 0, 0, fov_radius-1, true);
	test(0, 0, 0, 0, fov_radius, true);
	test(0, 0, 0, 0, fov_radius+1, false);
	test(0, 0, 0, fov_radius/2, fov_radius/2, true);
	test(0, 0, 0, fov_radius, fov_radius, false);
	test(0, 0, fov_max_angle-1, fov_radius, 0, true);
	test(0, 0, fov_max_angle, fov_radius, 0, true);
	test(0, 0, fov_max_angle+1, fov_radius, 0, false);
	test(0, 0, 360 - fov_max_angle + 1, fov_radius, 0, true);
	test(0, 0, 360 - fov_max_angle, fov_radius, 0, true);
	test(0, 0, 360 - fov_max_angle - 1, fov_radius, 0, false);

	Entity::set_fov_radius(initial_fov_radius);
	Entity::set_fov_max_angle(initial_fov_max_angle);
}

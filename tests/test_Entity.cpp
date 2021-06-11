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
	/**
	 * Single test case as constructor currently does not check bounds.
	 **/

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
	/**
	 * Test case format: start_angle, delta_angle, expected_result
	 **/

	Entity entity(Vec2(0.0f, 0.0f), 0.0f, 0.0f);

	// Test 0: 0, 0, 0
	entity.rotate(0.0f);
	assert(approx_equal(entity.angle, 0.0f));

	// Test 1: 0, 90, 90
	entity.angle = 0.0f;
	entity.rotate(90.0f);
	assert(approx_equal(entity.angle, 90.0f));

	// Test 2: 0, -90, 270
	entity.angle = 0.0f;
	entity.rotate(-90.0f);
	assert(approx_equal(entity.angle, 270.0f));

	// Test 3: 270, 90, 0
	entity.angle = 270.0f;
	entity.rotate(90.0f);
	assert(approx_equal(entity.angle, 0.0f));

	// Test 4: 0, 450, 90
	entity.angle = 0.0f;
	entity.rotate(450.0f);
	assert(approx_equal(entity.angle, 90.0f));

	// Test 5: 0, -450, 270
	entity.angle = 0.0f;
	entity.rotate(-450.0f);
	assert(approx_equal(entity.angle, 270.0f));
}

void test_rotate_towards()
{
	// @TODO Figure out how to test this
}

void test_update_pos()
{
	/**
	 * Test case format:
	 * start_x, start_y, angle, velocity, expected_x, expected_y
	 *
	 * These test cases assume the entity travels for one second.
	 **/

	Entity entity(Vec2(0.0f, 0.0f), 0.0f, 0.0f);

	// Test 0: 0, 0, 0, 0, 0, 0
	entity.update_pos(1.0f * US_PER_SECOND);
	assert(approx_equal(entity.pos.x, 0.0f));
	assert(approx_equal(entity.pos.y, 0.0f));

	// Test 1: 0, 0, 0, 1, 1, 0
	entity.pos.x = 0.0f;
	entity.pos.y = 0.0f;
	entity.angle = 0.0f;
	entity.velocity = 1.0f;
	entity.update_pos(1.0f * US_PER_SECOND);
	assert(approx_equal(entity.pos.x, 1.0f));
	assert(approx_equal(entity.pos.y, 0.0f));

	// Test 2: 0, 0, 45, sqrt(2), 1, 1
	entity.pos.x = 0.0f;
	entity.pos.y = 0.0f;
	entity.angle = 45.0f;
	entity.velocity = sqrt(2);
	entity.update_pos(1.0f * US_PER_SECOND);
	assert(approx_equal(entity.pos.x, 1.0f));
	assert(approx_equal(entity.pos.y, 1.0f));

	// Test 3: 1, 0, 180, 1, 0, 0
	entity.pos.x = 1.0f;
	entity.pos.y = 0.0f;
	entity.angle = 180.0f;
	entity.velocity = 1.0f;
	entity.update_pos(1.0f * US_PER_SECOND);
	assert(approx_equal(entity.pos.x, 0.0f));
	assert(approx_equal(entity.pos.y, 0.0f));

	// Test 4: 1, 0, 135, sqrt(2), 0, 1
	entity.pos.x = 1.0f;
	entity.pos.y = 0.0f;
	entity.angle = 135.0f;
	entity.velocity = sqrt(2);
	entity.update_pos(1.0f * US_PER_SECOND);
	assert(approx_equal(entity.pos.x, 0.0f));
	assert(approx_equal(entity.pos.y, 1.0f));

	// Test 5: 0, 0, 90, 1, 0, 1
	entity.pos.x = 0.0f;
	entity.pos.y = 0.0f;
	entity.angle = 90.0f;
	entity.velocity = 1.0f;
	entity.update_pos(1.0f * US_PER_SECOND);
	assert(approx_equal(entity.pos.x, 0.0f));
	assert(approx_equal(entity.pos.y, 1.0f));

	// Test 6: 1, 1, 225, sqrt(2), 0, 0
	entity.pos.x = 1.0f;
	entity.pos.y = 1.0f;
	entity.angle = 225.0f;
	entity.velocity = sqrt(2);
	entity.update_pos(1.0f * US_PER_SECOND);
	assert(approx_equal(entity.pos.x, 0.0f));
	assert(approx_equal(entity.pos.y, 0.0f));

	// Test 7: 0, 1, 270, 1, 0, 0
	entity.pos.x = 0.0f;
	entity.pos.y = 1.0f;
	entity.angle = 270.0f;
	entity.velocity = 1.0f;
	entity.update_pos(1.0f * US_PER_SECOND);
	assert(approx_equal(entity.pos.x, 0.0f));
	assert(approx_equal(entity.pos.y, 0.0f));

	// Test 8: 0, 1, 315, sqrt(2), 1, 0
	entity.pos.x = 0.0f;
	entity.pos.y = 1.0f;
	entity.angle = 315.0f;
	entity.velocity = sqrt(2);
	entity.update_pos(1.0f * US_PER_SECOND);
	assert(approx_equal(entity.pos.x, 1.0f));
	assert(approx_equal(entity.pos.y, 0.0f));
}

void test_in_fov()
{
	/**
	 * Test case format:
	 * entity0.pos.x, entity0.pos.y, entity0_angle, entity1.pos.x, entity1.pos.y, expected_result
	 **/

	Entity entity0(Vec2(0.0f, 0.0f), 0.0f, 0.0f);	// Looker
	Entity entity1(Vec2(0.0f, 0.0f), 0.0f, 0.0f);	// Target

	const float fov_radius = entity0.get_fov_radius();
	const float fov_max_angle = entity0.get_fov_max_angle();

	// Test 0: 0, 0, 0, 0, 0, true
	assert(entity0.in_fov(entity1) == true);

	// Test 1: 0, 0, 180, 0, 0, false
	entity0.angle = 180.0f;
	assert(entity0.in_fov(entity1) == false);

	// @TODO Add other test cases
	// Test 2: 0, 0, 0, 1, 0, true
	entity0.pos.x = 0.0f;
	entity0.pos.y = 0.0f;
	entity0.angle = 0.0f;
	entity1.pos.x = 1.0f;
	entity1.pos.y = 0.0f;
	assert(entity0.in_fov(entity1) == true);

	// Test 3: 0, 0, 0, 0, 0, true

	// Test 4: 0, 0, 0, 0, 0, true

	// Test 5: 0, 0, 0, 0, 0, true

	// Test 6: 0, 0, 0, 0, 0, true

	// Test 7: 0, 0, 0, 0, 0, true

	// Test 8: 0, 0, 0, 0, 0, true
}

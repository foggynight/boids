// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <cassert>
#include <vector>

#include "Boid.hpp"
#include "Vec2.hpp"

extern "C" {
#include "util.h"
}

void Boid::get_neighbors(std::vector<Boid>& boid_vec, std::vector<Boid *>& neighbor_vec) const
{
	for (auto& boid : boid_vec)
		if (&boid != this && in_fov(boid))
			neighbor_vec.push_back(&boid);
}

float Boid::alignment(const std::vector<Boid *>& neighbor_vec) const
{
	assert(neighbor_vec.size() > 0);

	Vec2 target_direction;
	for (const auto& neighbor : neighbor_vec)
		target_direction += Vec2(neighbor->angle);
	target_direction.normalize();

	return target_direction.angle();
}

float Boid::cohesion(const std::vector<Boid *>& neighbor_vec) const
{
	assert(neighbor_vec.size() > 0);

	Vec2 mean_pos;
	for (const auto& neighbor : neighbor_vec)
		mean_pos += neighbor->pos;
	mean_pos /= neighbor_vec.size();

	return (mean_pos - pos).angle();
}

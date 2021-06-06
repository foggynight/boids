// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#ifndef BOID_HPP
#define BOID_HPP

#include <vector>

#include "Entity.hpp"

class Boid : public Entity
{
public:
	using Entity::Entity;

	/**
	 * Get the neighbors of this boid. The neighbors vector is
	 * cleared and then filled with pointers to the neighbors.
	 *
	 * @param entity_vec	Vector to derive neighbors from
	 * @param neighbor_vec	Vector to fill with neighbor pointers
	 **/
	void get_neighbors(std::vector<Boid>& boid_vec, std::vector<Boid *>& neighbor_vec);

	/**
	 * Align with neighbors by gradually turning towards the average
	 * angle of the boids within this boid's FOV.
	 **/
	void align_with_neighbors(const std::vector<Boid *>& neighbor_vec, int delta_time_us);

	void cohere_with_neighbors(const std::vector<Boid *>& neighbor_vec, int delta_time_us);
};

#endif	// BOID_HPP

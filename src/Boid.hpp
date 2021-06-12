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
	 * Get the neighbors of this boid from the boid vector. The neighbors vector
	 * is cleared and then filled with pointers to the neighboring boids.
	 *
	 * @param entity_vec	Vector to derive neighbors from
	 * @param neighbor_vec	Vector to fill with neighbor pointers
	 **/
	void get_neighbors(std::vector<Boid>& boid_vec, std::vector<Boid *>& neighbor_vec) const;

	/**
	 * Get a Vec2 representing the combined directions of this boid's neighbors.
	 *
	 * @param neighbor_vec Vector of neighbors
	 *
	 * @return Vec2 representing the combined directions of neighbors
	 **/
	Vec2 alignment(const std::vector<Boid *>& neighbor_vec) const;

	/**
	 * Get a Vec2 representing the direction pointing from this boid to the
	 * average position of its neighbors.
	 *
	 * @param neighbor_vec Vector of neighbors
	 *
	 * @return Vec2 representing the direction from this boid to the average
	 * position of its neighbors
	 **/
	Vec2 cohesion(const std::vector<Boid *>& neighbor_vec) const;
};

#endif	// BOID_HPP

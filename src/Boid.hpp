// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#ifndef BOID_HPP
#define BOID_HPP

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
};

#endif	// BOID_HPP

// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#ifndef ENTITY_HPP
#define ENTITY_HPP

/**
 * Entity type representing an entity in the world. They are contained
 * within a rectangle with a width and height. They have a position, and
 * move through the world based on their angle and velocity.
 *
 * @member x	x position at the center of the entity
 * @member y	y position at the center of the entity
 *
 * @member angle	Angle in degrees relative to the right facing
 *	x-axis with a clockwise rotation
 * @member velocity	Forward velocity in pixels per second
 **/
class Entity
{
public:
	float x;
	float y;

	float angle;
	float velocity;

public:
	Entity(float x, float y, float angle, float velocity)
		: x(x), y(y), angle(angle), velocity(velocity) {}

	/**
	 * Update the position of this entity based on its angle and
	 * velocity.
	 *
	 * @param delta_time_us	Microseconds since previous update
	 **/
	void update_pos(int delta_time_us);

	int get_width();
	int get_height();

	float get_fov_radius();
	float get_fov_max_angle();

protected:
	bool in_fov(Entity& target);
};

#endif	// ENTITY_HPP

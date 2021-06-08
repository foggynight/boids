// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#ifndef ENTITY_HPP
#define ENTITY_HPP

/**
 * Entity type representing an entity in the world.
 *
 * An entity is contained within a rectangle with a width and height, it
 * has a position, and moves through the world in a forward direction
 * based on its angle and velocity.
 *
 * @member x	x position at the center of this entity
 * @member y	y position at the center of this entity
 *
 * @member angle	Angle in degrees relative to the right facing x-axis
 *		with a clockwise rotation
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
	 * Rotate this entity by adding delta_angle to its angle member, and
	 * resolving the overflows.
	 *
	 * @param delta_angle	Number of degrees to add to the angle member
	 **/
	void rotate(float delta_angle);

	/**
	 * Gradually rotate this entity about its center, based on the
	 * entity rotation speed and time passed.
	 *
	 * @param target_angle	Angle to rotate this entity towards
	 * @param delta_time_us	Microseconds since previous update
	 **/
	void rotate_towards(float target_angle, int delta_time_us);

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

	// @TODO Explain FOV
	bool in_fov(Entity& target);
};

#endif	// ENTITY_HPP

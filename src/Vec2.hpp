// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#ifndef VEC2_HPP
#define VEC2_HPP

/**
 * Vector lying in the x-y plane.
 *
 * @note As is convention in programming, the x-axis increases to the right, and
 * y-axis increases downwards.
 *
 * @note Angles are measured in degrees relative to the right facing x-axis with
 * a clockwise rotation.
 **/
class Vec2
{
public:
	float x;	// x component of the vector
	float y;	// y component of the vector

public:
	/**
	 * Construct a Vec2 with x and y set to zero.
	 **/
	Vec2();

	/**
	 * Construct a Vec2 from an angle by converting the angle into a unit
	 * vector, setting x and y to the components of the unit vector.
	 *
	 * @param angle	Angle to convert into a unit vector
	 **/
	Vec2(float angle);

	/**
	 * Construct a Vec2 with x and y set directly to the parameter values.
	 *
	 * @param x	x component of the vector
	 * @param y	y component of the vector
	 **/
	Vec2(float x, float y);

	/**
	 * Get the angle of this Vec2.
	 *
	 * @return Angle of this Vec2
	 **/
	float angle() const;

	/**
	 * Get the length of this Vec2.
	 *
	 * @return Length of this Vec2
	 **/
	float length() const;

	/**
	 * Normalize this Vec2.
	 *
	 * @return This Vec2
	 *
	 * @note For the sake of the simulation, this member function also converts
	 * the zero vector to a vector of length one pointing along the positive
	 * x-axis.
	 **/
	Vec2& normalize();

	/**
	 * Perform vector addition on two Vec2s, creating a new Vec2.
	 *
	 * @param target	Vec2 to add to this Vec2
	 *
	 * @return Resultant Vec2
	 **/
	Vec2 operator+(const Vec2& target) const;

	/**
	 * Perform vector subtraction on two Vec2s, creating a new Vec2.
	 *
	 * @param target	Vec2 to subtract from this Vec2
	 *
	 * @return Resultant Vec2
	 **/
	Vec2 operator-(const Vec2& target) const;

	/**
	 * Perform scalar multiplication on this Vec2, creating a new Vec2.
	 *
	 * @param target	Scalar to multiply this Vec2 by
	 *
	 * @return Resultant Vec2
	 **/
	Vec2 operator*(float target) const;

	/**
	 * Perform scalar division on this Vec2, creating a new Vec2.
	 *
	 * @param target	Scalar to divide this Vec2 by
	 *
	 * @return Resultant Vec2
	 **/
	Vec2 operator/(float target) const;

	/**
	 * Perform vector addition on two Vec2s, modifying this Vec2.
	 *
	 * @param target	Vec2 to add to this Vec2
	 *
	 * @return This Vec2
	 **/
	Vec2& operator+=(const Vec2& target);

	/**
	 * Perform vector subtraction on two Vec2s, modifying this Vec2.
	 *
	 * @param target	Vec2 to subtract from this Vec2
	 *
	 * @return This Vec2
	 **/
	Vec2& operator-=(const Vec2& target);

	/**
	 * Perform scalar multiplication on this Vec2, modifying this Vec2.
	 *
	 * @param target	Scalar to multiply this Vec2 by
	 *
	 * @return This Vec2
	 **/
	Vec2& operator*=(float target);

	/**
	 * Perform scalar division on this Vec2, modifying this Vec2.
	 *
	 * @param target	Scalar to divide this Vec2 by
	 *
	 * @return This Vec2
	 **/
	Vec2& operator/=(float target);
};

#endif	// VEC2_HPP

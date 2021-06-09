// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#ifndef VEC2_HPP
#define VEC2_HPP

class Vec2 {
public:
	float x;
	float y;

public:
	Vec2(float x = 0.0f, float y = 0.0f);

	/**
	 * Get the angle of the vector relative to the right facing x-axis, with a
	 * clockwise rotation starting at the x-axis.
	 *
	 * @return Angle of the vector
	 **/
	float angle() const;

	/**
	 * Get the length of the vector.
	 *
	 * @return Length of the vector
	 **/
	float length() const;

	Vec2 operator+(const Vec2& target) const;
	Vec2 operator-(const Vec2& target) const;
	Vec2 operator*(float target) const;
	Vec2 operator/(float target) const;

	Vec2& operator+=(const Vec2& target);
	Vec2& operator-=(const Vec2& target);
	Vec2& operator*=(float target);
	Vec2& operator/=(float target);
};

#endif	// VEC2_HPP

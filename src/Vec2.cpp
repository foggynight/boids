// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include <cmath>

#include "Vec2.hpp"

extern "C" {
#include "util.h"
}

Vec2::Vec2(float x, float y)
	: x(x), y(y) {}

float Vec2::angle() const
{
	float theta = 0.0f;

	if (x == 0.0f && y == 0.0f)
		theta = 0.0f;
	else if (x == 0.0f) {
		if (y > 0.0f)
			theta = 90.0f;
		else	// y < 0.0f
			theta = 270.0f;
	}
	else if (y == 0.0f) {
		if (x > 0.0f)
			theta = 0.0f;
		else	// x < 0.0f
			theta = 180.0f;
	}
	else {
		theta = rad_to_deg(atan(y / x));
		if (x < 0.0f)
			theta += 180.0f;
		else if (y < 0.0f)
			theta += 360.0f;
	}

	return theta;
}

float Vec2::length() const
{
	return sqrt(pow(x, 2) + pow(y, 2));
}

Vec2 Vec2::operator+(const Vec2& target) const
{
	return Vec2(x + target.x, y + target.y);
}

Vec2 Vec2::operator-(const Vec2& target) const
{
	return Vec2(x - target.x, y - target.y);
}

Vec2 Vec2::operator*(float target) const
{
	return Vec2(x * target, y * target);
}

Vec2 Vec2::operator/(float target) const
{
	return Vec2(x / target, y / target);
}

Vec2& Vec2::operator+=(const Vec2& target)
{
	x += target.x;
	y += target.y;
	return *this;
}

Vec2& Vec2::operator-=(const Vec2& target)
{
	x -= target.x;
	y -= target.y;
	return *this;
}

Vec2& Vec2::operator*=(float target)
{
	x *= target;
	y *= target;
	return *this;
}

Vec2& Vec2::operator/=(float target)
{
	x /= target;
	y /= target;
	return *this;
}

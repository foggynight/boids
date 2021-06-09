// Copyright (C) 2021 Robert Coffey
// Released under the GPLv2 license

#include "Vec2.hpp"

Vec2::Vec2(float x, float y)
	: x(x), y(y) {}

Vec2 Vec2::operator+(const Vec2& target)
{
	return Vec2(x + target.x, y + target.y);
}

Vec2 Vec2::operator-(const Vec2& target)
{
	return Vec2(x - target.x, y - target.y);
}

Vec2 Vec2::operator*(float target)
{
	return Vec2(x * target, y * target);
}

Vec2 Vec2::operator/(float target)
{
	return Vec2(x / target, y / target);
}

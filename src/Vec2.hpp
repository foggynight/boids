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
	Vec2 operator+(const Vec2& target);
	Vec2 operator-(const Vec2& target);
	Vec2 operator*(float target);
	Vec2 operator/(float target);
};

#endif	// VEC2_HPP

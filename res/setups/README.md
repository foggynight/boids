# boid-setups

This directory contains setups for the boids. A setup defines the initial
parameters of a set of boids such as their position, angle, and velocity. Setups
are defined using CSV files representing a table such as the following:

| x-pos | y-pos | angle | velocity |
|-------|-------|-------|----------|
| 0.0   | 0.0   | 0     | 1.0      |
| 10.0  | 10.0  | 180   | -1.0     |

- `x-pos`: x position represented as a decimal number
- `y-pos`: y position represented as a decimal number
- `angle`: Angle in degrees relative to the right-facing x-axis with a clockwise
  rotation represented as an integer
- `velocity`: Forward velocity represented as a decimal number

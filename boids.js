// boids.scm - Boids algorithm demonstration.
// Copyright (C) 2022 Robert Coffey
// Released under the GPLv3.

// config //////////////////////////////////////////////////////////////////////

const BOID_COLOR = "lightgreen"
const BOID_COUNT = 100

const CANVAS_WIDTH = 800
const CANVAS_HEIGHT = 600

const NEIGH_DIST = 32

const NEIGH_ALIGN_ACCEL = 0.01

const NEIGH_CLUSTER_ACCEL = 0.01

const NEIGH_AVOID_DIST = 10
const NEIGH_AVOID_ACCEL = 0.1

const SPEED_LIMIT = 2

const SLEEP_TIME = 1 / 60

const WALL_DIST = 100
const WALL_ACCEL = 0.1

// utility /////////////////////////////////////////////////////////////////////

const rand_float = max => max * Math.random()
const sleep = ms => new Promise(resolve => setTimeout(resolve, ms))

// vec2 ////////////////////////////////////////////////////////////////////////

class Vec2 {
    constructor(x, y) {
        this.x = x
        this.y = y
    }

    add(vec) { this.x += vec.x; this.y += vec.y }

    length() {
        return Math.sqrt(Math.pow(this.x, 2) + Math.pow(this.y, 2))
    }

    normalize() {
        const len = this.length()
        if (len !== 0) {
            this.x /= len
            this.y /= len
        }
    }
}

const Vec2_sub = (v1, v2) => new Vec2(v2.x - v1.x, v2.y - v1.y)

// boid ////////////////////////////////////////////////////////////////////////

class Boid {
    constructor(id) {
        this.id = id
        this.pos = new Vec2(rand_float(CANVAS_WIDTH), rand_float(CANVAS_HEIGHT))
        this.vel = new Vec2(rand_float(SPEED_LIMIT) - SPEED_LIMIT / 2,
                            rand_float(SPEED_LIMIT) - SPEED_LIMIT / 2)
    }

    speed() { return Math.sqrt(Math.pow(this.vel.x, 2)
                               + Math.pow(this.vel.y, 2)) }
    move() { this.pos.add(this.vel) }
    accelerate(acc) { this.vel.add(acc) }

    is_neighbor(boid) {
        if (this.id === boid.id) return false
        const dist = Vec2_sub(boid.pos, this.pos).length()
        return dist <= NEIGH_DIST
    }

    get_neighbors(boids) {
        const neighbors = []
        boids.forEach(boid => {
            if (this.is_neighbor(boid))
                neighbors.push(boid)
        })
        return neighbors
    }

    move_toward_neighbors(neighbors) {
        const pos = new Vec2(0, 0)
        for (let neigh of neighbors)
            pos.add(neigh.pos)
        pos.x /= neighbors.length
        pos.y /= neighbors.length
        const delta_pos = Vec2_sub(this.pos, pos)
        delta_pos.normalize()
        delta_pos.x *= NEIGH_CLUSTER_ACCEL
        delta_pos.y *= NEIGH_CLUSTER_ACCEL
        this.accelerate(delta_pos)
    }

    align_with_neighbors(neighbors) {
        const vel = new Vec2(0, 0)
        for (let neigh of neighbors)
            vel.add(neigh.vel)
        vel.x /= neighbors.length
        vel.y /= neighbors.length
        const delta_vel = Vec2_sub(this.vel, vel)
        delta_vel.normalize()
        delta_vel.x *= NEIGH_ALIGN_ACCEL
        delta_vel.y *= NEIGH_ALIGN_ACCEL
        this.accelerate(delta_vel)
    }

    avoid_neighbors(neighbors) {
        const acc = new Vec2(0, 0)
        for (let neigh of neighbors) {
            const delta_pos = Vec2_sub(this.pos, neigh.pos)
            if (delta_pos.length() < NEIGH_AVOID_DIST) {
                acc.x += 1 - delta_pos.x
                acc.y += 1 - delta_pos.y
            }
        }
        acc.normalize()
        acc.x *= NEIGH_AVOID_ACCEL
        acc.y *= NEIGH_AVOID_ACCEL
        this.accelerate(acc)
    }

    avoid_walls() {
        const acc = new Vec2(0, 0)
        if (this.pos.x < WALL_DIST) acc.x = WALL_ACCEL
        else if (this.pos.x > CANVAS_WIDTH - WALL_DIST) acc.x = -WALL_ACCEL
        if (this.pos.y < WALL_DIST) acc.y = WALL_ACCEL
        else if (this.pos.y > CANVAS_HEIGHT - WALL_DIST) acc.y = -WALL_ACCEL
        this.accelerate(acc)
    }

    limit_speed() {
        const speed = this.speed()
        if (speed > SPEED_LIMIT) {
            this.vel.x = this.vel.x / speed * SPEED_LIMIT
            this.vel.y = this.vel.y / speed * SPEED_LIMIT
        }
    }

    draw() {
        ctx.strokeStyle = BOID_COLOR
        ctx.beginPath()
        ctx.moveTo(this.pos.x, this.pos.y)
        ctx.lineTo(this.pos.x + this.vel.x * 10, this.pos.y + this.vel.y * 10)
        ctx.stroke()
    }
}

// main ////////////////////////////////////////////////////////////////////////

const canvas = document.getElementById('canvas')
canvas.width = CANVAS_WIDTH
canvas.height = CANVAS_HEIGHT
const ctx = canvas.getContext('2d')

async function main() {
    const boids = []
    for (let i = 0; i < BOID_COUNT; ++i)
        boids.push(new Boid(i))
    while (true) {
        for (let boid of boids) {
            neighbors = boid.get_neighbors(boids)
            if (neighbors.length > 0) {
                boid.align_with_neighbors(neighbors)
                boid.move_toward_neighbors(neighbors)
                boid.avoid_neighbors(neighbors)
            }
            boid.avoid_walls()
            boid.limit_speed()
            boid.move()
        }
        ctx.clearRect(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT)
        boids.forEach(boid => boid.draw())
        await sleep(SLEEP_TIME)
    }
}

main()

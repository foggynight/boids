// boids.js - Boids algorithm written in JavaScript.
// Copyright (C) 2022 Robert Coffey
// Released under the GPLv3.

// config //////////////////////////////////////////////////////////////////////

config = {
    BOID_COLOR: "lightgreen",
    BOID_COUNT: parseInt(document.getElementById('count').value),

    CANVAS_WIDTH: document.body.offsetWidth,
    CANVAS_HEIGHT: Math.floor(document.body.offsetWidth * 3 / 4),

    NEIGH_DIST: parseFloat(document.getElementById('dist').value),
    NEIGH_ALIGN_ACCEL: parseFloat(document.getElementById('align').value),
    NEIGH_CLUSTER_ACCEL: parseFloat(document.getElementById('cluster').value),
    NEIGH_AVOID_DIST: parseFloat(document.getElementById('avoid-dist').value),
    NEIGH_AVOID_ACCEL: parseFloat(document.getElementById('avoid-accel').value),

    SPEED_LIMIT: parseFloat(document.getElementById('limit').value),

    SLEEP_TIME: 1 / 60,

    WALL_DIST: parseFloat(document.getElementById('wall-dist').value),
    WALL_ACCEL: parseFloat(document.getElementById('wall-accel').value),
}

document.getElementById('count').oninput = () => {
    config.BOID_COUNT = parseInt(document.getElementById('count').value)
    document.getElementById('output-count').value = config.BOID_COUNT

    const delta = config.BOID_COUNT - boids.length
    if (delta < 0) for (let i = 0; i < -delta; ++i) boids.pop()
    else if (delta > 0) for (let i = 0; i < delta; ++i) boids.push(new Boid())
}

function hook_input(elem_id, config_var) {
    document.getElementById(elem_id).oninput = () => {
        config[config_var] = parseFloat(document.getElementById(elem_id).value)
        document.getElementById('output-' + elem_id).value = config[config_var]
    }
}

hook_input('dist', 'NEIGH_DIST')
hook_input('align', 'NEIGH_ALIGN_ACCEL')
hook_input('cluster', 'NEIGH_CLUSTER_ACCEL')
hook_input('avoid-dist', 'NEIGH_AVOID_DIST')
hook_input('avoid-accel', 'NEIGH_AVOID_ACCEL')
hook_input('limit', 'SPEED_LIMIT')
hook_input('wall-dist', 'WALL_DIST')
hook_input('wall-accel', 'WALL_ACCEL')

// utility /////////////////////////////////////////////////////////////////////

const rand_float = max => max * Math.random()
const sleep = ms => new Promise(resolve => setTimeout(resolve, ms))

// vec2 ////////////////////////////////////////////////////////////////////////

class Vec2 {
    constructor(x, y) { this.x = x; this.y = y }
    mul(scalar) { this.x *= scalar; this.y *= scalar }
    div(scalar) { this.x /= scalar; this.y /= scalar }
    add_vec(vec) { this.x += vec.x; this.y += vec.y }
    length() { return Math.sqrt(Math.pow(this.x, 2) + Math.pow(this.y, 2)) }

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
    constructor() {
        this.pos = new Vec2(rand_float(config.CANVAS_WIDTH),
                            rand_float(config.CANVAS_HEIGHT))
        this.vel = new Vec2(rand_float(config.SPEED_LIMIT)-config.SPEED_LIMIT/2,
                            rand_float(config.SPEED_LIMIT)-config.SPEED_LIMIT/2)
    }

    speed() { return this.vel.length() }
    move() { this.pos.add_vec(this.vel) }
    accelerate(acc) { this.vel.add_vec(acc) }

    is_neighbor(boid) {
        if (this === boid) return false
        const dist = Vec2_sub(boid.pos, this.pos).length()
        return dist <= config.NEIGH_DIST
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
            pos.add_vec(neigh.pos)
        pos.div(neighbors.length)
        const delta_pos = Vec2_sub(this.pos, pos)
        delta_pos.normalize()
        delta_pos.mul(config.NEIGH_CLUSTER_ACCEL)
        this.accelerate(delta_pos)
    }

    align_with_neighbors(neighbors) {
        const vel = new Vec2(0, 0)
        for (let neigh of neighbors)
            vel.add_vec(neigh.vel)
        vel.div(neighbors.length)
        const delta_vel = Vec2_sub(this.vel, vel)
        delta_vel.normalize()
        delta_vel.mul(config.NEIGH_ALIGN_ACCEL)
        this.accelerate(delta_vel)
    }

    avoid_neighbors(neighbors) {
        const acc = new Vec2(0, 0)
        for (let neigh of neighbors) {
            const delta_pos = Vec2_sub(this.pos, neigh.pos)
            if (delta_pos.length() < config.NEIGH_AVOID_DIST) {
                acc.x += 1 - delta_pos.x
                acc.y += 1 - delta_pos.y
            }
        }
        acc.normalize()
        acc.mul(config.NEIGH_AVOID_ACCEL)
        this.accelerate(acc)
    }

    avoid_walls() {
        const acc = new Vec2(0, 0)
        if (this.pos.x < config.WALL_DIST)
            acc.x = config.WALL_ACCEL
        else if (this.pos.x > config.CANVAS_WIDTH - config.WALL_DIST)
            acc.x = -config.WALL_ACCEL
        if (this.pos.y < config.WALL_DIST)
            acc.y = config.WALL_ACCEL
        else if (this.pos.y > config.CANVAS_HEIGHT - config.WALL_DIST)
            acc.y = -config.WALL_ACCEL
        this.accelerate(acc)
    }

    limit_speed() {
        const speed = this.speed()
        if (speed > config.SPEED_LIMIT)
            this.vel.div(speed * config.SPEED_LIMIT)
    }

    draw() {
        ctx.strokeStyle = config.BOID_COLOR
        ctx.beginPath()
        ctx.moveTo(this.pos.x, this.pos.y)
        ctx.lineTo(this.pos.x + this.vel.x * 10, this.pos.y + this.vel.y * 10)
        ctx.stroke()
    }
}

// main ////////////////////////////////////////////////////////////////////////

const canvas = document.getElementById('boids')
canvas.width = config.CANVAS_WIDTH
canvas.height = config.CANVAS_HEIGHT
const ctx = canvas.getContext('2d')

const boids = []

async function main() {
    for (let i = 0; i < config.BOID_COUNT; ++i)
        boids.push(new Boid())
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
        ctx.clearRect(0, 0, config.CANVAS_WIDTH, config.CANVAS_HEIGHT)
        boids.forEach(boid => boid.draw())
        await sleep(config.SLEEP_TIME)
    }
}

main()

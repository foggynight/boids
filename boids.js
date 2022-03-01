// boids.scm - Boids algorithm demonstration.
// Copyright (C) 2022 Robert Coffey
// Released under the GPLv3.

// config //////////////////////////////////////////////////////////////////////

const BOID_COLOR = "lightgreen"
const BOID_COUNT = 100

const CANVAS_WIDTH = 800
const CANVAS_HEIGHT = 600

const NEIGHBOR_DIST = 100

const SPEED_LIMIT = 4

const SLEEP_TIME = 1 / 60

const WALL_DIST = 64
const WALL_ACCEL = 0.04

// utility /////////////////////////////////////////////////////////////////////

const rand_float = max => max * Math.random()
const sleep = ms => new Promise(resolve => setTimeout(resolve, ms))

// boid ////////////////////////////////////////////////////////////////////////

class Boid {
    constructor() {
        this.x = rand_float(CANVAS_WIDTH)
        this.y = rand_float(CANVAS_HEIGHT)
        this.vx = rand_float(SPEED_LIMIT) - SPEED_LIMIT / 2
        this.vy = rand_float(SPEED_LIMIT) - SPEED_LIMIT / 2
    }

    speed() { return Math.sqrt(Math.pow(this.vx, 2) + Math.pow(this.vy, 2)) }
    update_position() { this.x += this.vx; this.y += this.vy }
    accelerate(ax, ay) { this.vx += ax; this.vy += ay }

    is_neighbor(boid) {
        if (this === boid) return false
        const dist = Math.sqrt(Math.pow(this.x - boid.x, 2)
                               + Math.pow(this.y - boid.y, 2))
        return dist <= NEIGHBOR_DIST
    }

    get_neighbors(boids) {
        const neighbors = []
        boids.forEach(boid => {
            if (this.is_neighbor(boid))
                neighbors.push(boid)
        })
        return neighbors
    }

    avoid_walls() {
        let ax = 0, ay = 0
        if (this.x < WALL_DIST) ax = WALL_ACCEL
        else if (this.x > CANVAS_WIDTH - WALL_DIST) ax = -WALL_ACCEL
        if (this.y < WALL_DIST) ay = WALL_ACCEL
        else if (this.y > CANVAS_HEIGHT - WALL_DIST) ay = -WALL_ACCEL
        this.accelerate(ax, ay)
    }

    limit_speed() {
        const speed = this.speed()
        if (speed > SPEED_LIMIT) {
            this.vx = this.vx / speed * SPEED_LIMIT
            this.vy = this.vy / speed * SPEED_LIMIT
        }
    }

    draw() {
        ctx.strokeStyle = BOID_COLOR
        ctx.beginPath()
        ctx.moveTo(this.x, this.y)
        ctx.lineTo(this.x + this.vx * 10, this.y + this.vy * 10)
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
        boids.push(new Boid())
    while (true) {
        boids.forEach(boid => {
            neighbors = boid.get_neighbors(boids)
            boid.avoid_walls()
            boid.limit_speed()
            boid.update_position()
        })
        ctx.clearRect(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT)
        boids.forEach(boid => boid.draw())
        await sleep(SLEEP_TIME)
    }
}

main()

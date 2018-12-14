package aoc.thirteen

public class Train {
    constructor(dir: Int, x: Int, y: Int) {
        this.direction = dir
        this.x = x
        this.y = y
        this.nextTurn = Turn.LEFT
    }

    var direction: Int
    var nextTurn: Turn
    var x: Int
    var y: Int

    fun makeTurn(turn: Turn) {
        this.direction += turn.dir
        correctDirection()
    }

    fun crossIntersection() {
        when (this.nextTurn) {
            Turn.LEFT -> {
                this.direction += this.nextTurn.dir
                this.nextTurn = Turn.STRAIGHT
            }
            Turn.STRAIGHT -> {
                this.nextTurn = Turn.RIGHT
            }
            Turn.RIGHT -> {
                this.direction += this.nextTurn.dir
                this.nextTurn = Turn.LEFT
            }
        }
        correctDirection()
    }

    fun correctDirection() {
        if (this.direction < 0) {
            this.direction = (this.direction + 4)
        } else if (this.direction > 3) {
            this.direction = (this.direction) % 4
        }
    }
}


public enum class Turn(val dir: Int) {
    LEFT(-1),
    STRAIGHT(0),
    RIGHT(1)
}

public enum class Field() {
    VERTICAL,
    HORIZONTAL,
    TOP_LEFT,
    TOP_RIGHT,
    EMPTY,
    INTERSECTION
}

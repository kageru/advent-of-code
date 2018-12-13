package aoc.day13

public class Cart() {
    constructor(d: Direction, x: Int, y: Int) {
        direction = d
        x = x
        y = y
    }
    var direction: Direction
    var nextTurn: Turn = Turn.LEFT
    var x: Int
    var y: Int
}


enum class Direction(val code: Int) {
    UP(0),
    RIGHT(1),
    DOWN(2)
    LEFT(3)
}


fun turnLeft(val oldDir: Direction): Direction {
    return Direction((oldDir.code + 3) % 4)
}

fun turnRight(val oldDir: Direction): Direction {
    return Direction((oldDir.code + 1) % 4)
}

fun crossIntersection(val cart: Cart): Cart {
    var retCart = cart
    retCart.direction = Direction((cart.direction.code + cart.nextTurn.dir - 1) % 4)
    retCart.nextTurn = Turn((cart.nextTurn.dir + 1) % 3)
    return retCart
}

enum class Turn(val dir: Int) {
    LEFT(0),
    STRAIGHT(1),
    RIGHT(2)
}

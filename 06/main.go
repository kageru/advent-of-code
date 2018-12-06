package main

import (
    "fmt"
    "io/ioutil"
    "strings"
    "strconv"
    //"math"
)

// tfw no built-in abs() for int
func Abs (x int) int {
    if x > 0 {
        return x
    } else {
        return x * -1
    }
}

func getBounds(nodes []Node) Point {
    outPoint := Point{ X: 0, Y: 0 }
    for _, n:= range(nodes) {
        if n.Pos.X > outPoint.X {
            outPoint.X = n.Pos.X
        }
        if n.Pos.Y > outPoint.Y {
            outPoint.Y = n.Pos.Y
        }
    }
    // It’s easier later on if we know that no points are touching the edges
    outPoint.X++
    outPoint.Y++
    return outPoint
}

type Point struct {
    X int
    Y int
}

type Node struct {
    Id int
    Pos Point
}

func ParseInput(input string) []Node {
    lines := strings.Split(input, "\n")
    output := make([]Node, len(lines))
    for i, line := range(lines) {
        coords := strings.Split(line, ", ")
        x, _ := strconv.Atoi(coords[0])
        y, _ := strconv.Atoi(coords[1])
        node := Node{ Id: i, Pos: Point{ X: x, Y: y } }
        output[i] = node
    }
    return output
}

func ManhattanDistance(first Point, second Point) int {
    return Abs(first.X - second.X) + Abs(first.Y - second.Y)
}

func main() {
    inFile, _ := ioutil.ReadFile("input")
    inString := string(inFile)

    points := ParseInput(inString);
    bounds := getBounds(points)
    fields := make([][]byte, bounds.X)
    for i := range(fields) {
        fields[i] = make([]byte, bounds.Y)
    }
    // Difference between node ID and corresponding occupied fields.
    // Must be greater than the input size
    diff := len(points) + 1
    fmt.Println(points)
    fmt.Println(fields)
    fmt.Println(diff)
    //fmt.Println(ManhattanDistance(p1, p2))
}


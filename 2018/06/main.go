package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
	//"math"
)

// tfw no built-in abs() for int
func Abs(x int) int {
	if x > 0 {
		return x
	}
	return x * -1
}

func getBounds(nodes []Node) Point {
	outPoint := Point{X: 0, Y: 0}
	for _, n := range nodes {
		if n.Pos.X > outPoint.X {
			outPoint.X = n.Pos.X
		}
		if n.Pos.Y > outPoint.Y {
			outPoint.Y = n.Pos.Y
		}
	}
	// Make sure no points are directly touching the edges
	outPoint.X++
	outPoint.Y++
	return outPoint
}

type Point struct {
	X int
	Y int
}

type Node struct {
	ID   byte
	Pos  Point
	Size int
}

func ParseInput(input string) []Node {
	lines := strings.Split(input, "\n")
	output := make([]Node, len(lines))
	for i, line := range lines {
		coords := strings.Split(line, ", ")
		x, _ := strconv.Atoi(coords[1])
		y, _ := strconv.Atoi(coords[0])
		node := Node{ID: byte(i), Pos: Point{X: x, Y: y}, Size: 0}
		output[i] = node
	}
	return output
}

func FindClosest(p Point, nodes []Node) byte {
	var closest Node
	minD := 9999999
	tied := false
	for _, node := range nodes {
		d := ManhattanDistance(node.Pos, p)
		if d < minD {
			minD = d
			closest = node
			tied = false
		} else if d == minD {
			tied = true
		}
	}
	if tied {
		return byte(255)
	}
	return closest.ID
}

func DistanceToAll(p Point, nodes []Node) int {
	total := 0
	for _, node := range nodes {
		total += ManhattanDistance(p, node.Pos)
	}
	return total
}

func CalculateTerritory(n Node, fields [][]byte) int {
	marker := n.ID
	size := 0
	boundX := len(fields) - 1
	boundY := len(fields[0]) - 1
	for x, outer := range fields {
		for y := range outer {
			if fields[x][y] == marker {
				if x == 0 || y == 0 || x == boundX || y == boundY {
					return 0
				}
				size++
			}
		}
	}
	return size
}

func ManhattanDistance(first Point, second Point) int {
	return Abs(first.X-second.X) + Abs(first.Y-second.Y)
}

func main() {
	inFile, _ := ioutil.ReadFile("input")
	inString := string(inFile)

	points := ParseInput(inString)
	bounds := getBounds(points)
	fields := make([][]byte, bounds.X)
	for i := range fields {
		fields[i] = make([]byte, bounds.Y)
	}

	// Part 1
	for x, outer := range fields {
		for y := range outer {
			fields[x][y] = FindClosest(Point{X: x, Y: y}, points)
		}
	}
	for i, node := range points {
		points[i].Size = CalculateTerritory(node, fields)
	}
	var biggest Node
	biggest.Size = 0
	for _, node := range points {
		if node.Size > biggest.Size {
			biggest = node
		}
		fields[node.Pos.X][node.Pos.Y] = node.ID + 100
	}
	fmt.Println(biggest)

	// Part 2
	closeEnough := 0
	for x, outer := range fields {
		for y := range outer {
			if DistanceToAll(Point{X: x, Y: y}, points) < 10000 {
				closeEnough++
			}
		}
	}
	fmt.Println(closeEnough)
}

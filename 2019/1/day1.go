package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

func main() {
	inFile, _ := ioutil.ReadFile("input")
	lines := strings.Split(string(inFile), "\n")

	fmt.Printf("Part 1: %d\n", part1(lines))
	fmt.Printf("Part 2: %d\n", part2(lines))
}

func part1(lines []string) int {
	fuel := 0
	for _, line := range lines {
		mass, _ := strconv.Atoi(line)
		fuel += cost(mass)
	}
	return fuel
}

func part2(lines []string) int {
	fuel := 0
	for _, line := range lines {
		mass, _ := strconv.Atoi(line)
		fuel += costRec(mass, 0)
	}
	return fuel
}

func cost(mass int) int {
	return mass/3 - 2
}

func costRec(mass int, acc int) int {
	c := cost(mass)
	if c <= 0 {
		return acc
	}
	return costRec(c, acc+c)
}

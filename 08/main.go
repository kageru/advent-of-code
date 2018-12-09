package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

type Node struct {
	Metadata []int
	Children []Node
}

func parseNode(input []int, start int) (parsed Node, end int) {
	numChildren := input[start]
	numMeta := input[start+1]
	start += 2
	for i := 0; i < numChildren; i++ {
		var child Node
		child, start = parseNode(input, start)
		parsed.Children = append(parsed.Children, child)
	}
	for i := 0; i < numMeta; i++ {
		parsed.Metadata = append(parsed.Metadata, input[start])
		start++
	}
	return parsed, start
}

func metaSum(root Node) (sum int) {
	for _, c := range root.Children {
		sum += metaSum(c)
	}
	for _, m := range root.Metadata {
		sum += m
	}
	return sum
}

func main() {
	inFile, _ := ioutil.ReadFile("input")
	rawInput := strings.Split(string(inFile), " ")
	input := make([]int, len(rawInput))
	for i, s := range rawInput {
		input[i], _ = strconv.Atoi(s)
	}
	root, _ := parseNode(input, 0)
	fmt.Println(metaSum(root))
}

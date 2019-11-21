package main

import (
    "fmt"
)

func completesTargetSequence(newElement byte) bool {
    if target[foundDigits] == newElement {
        foundDigits++
        if foundDigits == len(target) {
            return true
        }
    } else {
        foundDigits = 0
    }
    return false
}

var target = [...]byte{9, 1, 9, 9, 0, 1}
var foundDigits = 0

func main() {

    // Passing turns as the length here doesn’t work because it initializes 1 million bytes as 0 instead of allocating space so I can append later.
    // RIP performance, but I consider this way more readable than the alternativ.
    recipes := make([]byte, 0)
    recipes = append(recipes, 3)
    recipes = append(recipes, 7)

    e1, e2 := 0, 1
    for true {
        firstRecipe, secondRecipe := recipes[e1], recipes[e2]
        newRecipe := firstRecipe + secondRecipe
        if newRecipe >= 10 {
            recipes = append(recipes, 1)
            newRecipe -= 10
            if completesTargetSequence(1) {
                break
            }
        }
        recipes = append(recipes, newRecipe)
        if completesTargetSequence(newRecipe) {
            break
        }
        e1 = (e1 + int(firstRecipe) + 1) % len(recipes)
        e2 = (e2 + int(secondRecipe) + 1) % len(recipes)
    }
    fmt.Println(len(recipes) - len(target))
}

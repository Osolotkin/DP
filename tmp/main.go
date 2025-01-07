package main

import (
	"fmt"
)

func foo() (int, error) {
	return 42, fmt.Errorf("math: square root of negative number")
}

func main() {

	val1, err := foo() + 1
	if err != nil {
		fmt.Println("Error in first foo call:", err)
	}

	fmt.Println("Error in first foo call:", err)

	val2, err := foo()
	if err != nil {
		fmt.Println("Error in second foo call:", err)
	}

	fmt.Println("First value:", val1)
	fmt.Println("Second value:", val2)
}

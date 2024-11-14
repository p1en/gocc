package main

import (
	"fmt"
	"os"
)

func main() {
	if len(os.Args) != 2 {
		fmt.Fprintf(os.Stderr, "%s: invalid number of arguments\n", os.Args[0])
		os.Exit(1)
	}

	fmt.Printf(".intel_syntax noprefix\n")
	fmt.Printf(".global main\n")
	fmt.Printf("main:\n")
	fmt.Printf("  mov rax, %s\n", os.Args[1])
	fmt.Printf("  ret\n")
}
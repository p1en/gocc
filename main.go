package main

import (
	"os"
)

func main() {
	if len(os.Args) != 2 {
		reportError("%s: invalid number of arguments", os.Args[0])
	}

	// Tokenize and parse.
	userInput = os.Args[1]
	token = tokenize()
	node := program()

	// Traverse the AST to emit assembly.
	codegen(node)
}

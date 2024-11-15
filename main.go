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
	prog := program()

	// Assign offsets to local variables.
	offset := 0
	for v := prog.locals; v != nil; v = v.next {
		offset += 8
		v.offset = offset
	}
	prog.stackSize = offset

	// Traverse the AST to emit assembly.
	codegen(prog)
}

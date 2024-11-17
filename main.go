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
	addType(prog)

	// Assign offsets to local variables.
	for fn := prog; fn != nil; fn = fn.next {
		offset := 0
		for vl := fn.locals; vl != nil; vl = vl.next {
			v := vl.variable
			offset += sizeOf(v.ty)
			v.offset = offset
		}
		fn.stackSize = offset
	}

	// Traverse the AST to emit assembly.
	codegen(prog)
}

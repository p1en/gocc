package main

import (
	"os"
)

func alignTo(n int, align int) int {
	return (n + align - 1) & ^(align - 1)
}

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
	for fn := prog.fns; fn != nil; fn = fn.next {
		offset := 0
		for vl := fn.locals; vl != nil; vl = vl.next {
			v := vl.variable
			offset += sizeOf(v.ty)
			v.offset = offset
		}
		fn.stackSize = alignTo(offset, 8)
	}

	// Traverse the AST to emit assembly.
	codegen(prog)
}

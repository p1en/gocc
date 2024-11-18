package main

import "os"

func readFile(path string) string {
	data, err := os.ReadFile(path)
	if err != nil {
		reportError("cannot open %s: %s", path, err)
	}

	// Make sure that the string ends with "\n\0".
	if len(data) == 0 || data[len(data)-1] != '\n' {
		data = append(data, '\n')
	}

	return string(data)
}

func main() {
	if len(os.Args) != 2 {
		reportError("%s: invalid number of arguments", os.Args[0])
	}

	// Tokenize and parse.
	filename = os.Args[1]
	userInput = readFile(os.Args[1])
	token = tokenize()
	prog := program()
	addType(prog)

	// Assign offsets to local variables.
	for fn := prog.fns; fn != nil; fn = fn.next {
		offset := 0
		for vl := fn.locals; vl != nil; vl = vl.next {
			v := vl.variable
			offset = alignTo(offset, v.ty.align)
			offset += sizeOf(v.ty)
			v.offset = offset
		}
		fn.stackSize = alignTo(offset, 8)
	}

	// Traverse the AST to emit assembly.
	codegen(prog)
}

package main

import "fmt"

var argreg1 = []string{"dil", "sil", "dl", "cl", "r8b", "r9b"}
var argreg2 = []string{"di", "si", "dx", "cx", "r8w", "r9w"}
var argreg4 = []string{"edi", "esi", "edx", "ecx", "r8d", "r9d"}
var argreg8 = []string{"rdi", "rsi", "rdx", "rcx", "r8", "r9"}

var labelseq int
var funcname string

// Pushes the given node's address to the stack.
func genAddr(node *Node) {
	switch node.kind {
	case ND_VAR:
		v := node.variable
		if v.isLocal {
			fmt.Printf("  lea rax, [rbp-%d]\n", v.offset)
			fmt.Printf("  push rax\n")
		} else {
			fmt.Printf("  push offset %s\n", v.name)
		}
		return
	case ND_DEREF:
		gen(node.lhs)
		return
	case ND_MEMBER:
		genAddr(node.lhs)
		fmt.Printf("  pop rax\n")
		fmt.Printf("  add rax, %d\n", node.member.offset)
		fmt.Printf("  push rax\n")
		return
	}

	errorTok(node.tok, "not an lvalue")
}

func genLval(node *Node) {
	if node.ty.kind == TY_ARRAY {
		errorTok(node.tok, "not an lvalue")
	}
	genAddr(node)
}

func load(ty *Type) {
	fmt.Printf("  pop rax\n")

	sz := sizeOf(ty)
	switch sz {
	case 1:
		fmt.Printf("  movsx rax, byte ptr [rax]\n")
	case 2:
		fmt.Printf("  movsx rax, word ptr [rax]\n")
	case 4:
		fmt.Printf("  movsxd rax, dword ptr [rax]\n")
	default:
		if sz != 8 {
			panic("sz != 8")
		}
		fmt.Printf("  mov rax, [rax]\n")
	}

	fmt.Printf("  push rax\n")
}

func store(ty *Type) {
	fmt.Printf("  pop rdi\n")
	fmt.Printf("  pop rax\n")

	if ty.kind == TY_BOOL {
		fmt.Printf("  cmp rdi, 0\n")
		fmt.Printf("  setne dil\n")
		fmt.Printf("  movzb rdi, dil\n")
	}

	sz := sizeOf(ty)
	switch sz {
	case 1:
		fmt.Printf("  mov [rax], dil\n")
	case 2:
		fmt.Printf("  mov [rax], di\n")
	case 4:
		fmt.Printf("  mov [rax], edi\n")
	default:
		if sz != 8 {
			panic("sz != 8")
		}
		fmt.Printf("  mov [rax], rdi\n")
	}

	fmt.Printf("  push rdi\n")
}

// Generate code for a given node.
func gen(node *Node) {
	switch node.kind {
	case ND_NULL:
		return
	case ND_NUM:
		if node.val >= -1<<31 && node.val <= 1<<31-1 {
			fmt.Printf("  push %d\n", node.val)
		} else {
			fmt.Printf("  movabs rax, %d\n", node.val)
			fmt.Printf("  push rax\n")
		}
		return
	case ND_EXPR_STMT:
		gen(node.lhs)
		fmt.Printf("  add rsp, 8\n")
		return
	case ND_VAR, ND_MEMBER:
		genAddr(node)
		if node.ty.kind != TY_ARRAY {
			load(node.ty)
		}
		return
	case ND_ASSIGN:
		genLval(node.lhs)
		gen(node.rhs)
		store(node.ty)
		return
	case ND_ADDR:
		genAddr(node.lhs)
		return
	case ND_DEREF:
		gen(node.lhs)
		if node.ty.kind != TY_ARRAY {
			load(node.ty)
		}
		return
	case ND_IF:
		seq := labelseq
		labelseq++
		if node.els != nil {
			gen(node.cond)
			fmt.Printf("  pop rax\n")
			fmt.Printf("  cmp rax, 0\n")
			fmt.Printf("  je  .Lelse%d\n", seq)
			gen(node.then)
			fmt.Printf("  jmp .Lend%d\n", seq)
			fmt.Printf(".Lelse%d:\n", seq)
			gen(node.els)
			fmt.Printf(".Lend%d:\n", seq)
		} else {
			gen(node.cond)
			fmt.Printf("  pop rax\n")
			fmt.Printf("  cmp rax, 0\n")
			fmt.Printf("  je  .Lend%d\n", seq)
			gen(node.then)
			fmt.Printf(".Lend%d:\n", seq)
		}
		return
	case ND_WHILE:
		seq := labelseq
		labelseq++
		fmt.Printf(".Lbegin%d:\n", seq)
		gen(node.cond)
		fmt.Printf("  pop rax\n")
		fmt.Printf("  cmp rax, 0\n")
		fmt.Printf("  je  .Lend%d\n", seq)
		gen(node.then)
		fmt.Printf("  jmp .Lbegin%d\n", seq)
		fmt.Printf(".Lend%d:\n", seq)
		return
	case ND_FOR:
		seq := labelseq
		labelseq++
		if node.init != nil {
			gen(node.init)
		}
		fmt.Printf(".Lbegin%d:\n", seq)
		if node.cond != nil {
			gen(node.cond)
			fmt.Printf("  pop rax\n")
			fmt.Printf("  cmp rax, 0\n")
			fmt.Printf("  je  .Lend%d\n", seq)
		}
		gen(node.then)
		if node.inc != nil {
			gen(node.inc)
		}
		fmt.Printf("  jmp .Lbegin%d\n", seq)
		fmt.Printf(".Lend%d:\n", seq)
		return
	case ND_BLOCK, ND_STMT_EXPR:
		for n := node.body; n != nil; n = n.next {
			gen(n)
		}
		return
	case ND_FUNCALL:
		nargs := 0
		for arg := node.args; arg != nil; arg = arg.next {
			gen(arg)
			nargs++
		}

		for i := nargs - 1; i >= 0; i-- {
			fmt.Printf("  pop %s\n", argreg8[i])
		}

		// We need to align RSP to a 16 byte boundary before calling a function because it is an ABI requirement.
		// RAX is set to 0 for variadic function.
		seq := labelseq
		labelseq++

		fmt.Printf("  mov rax, rsp\n")
		fmt.Printf("  and rax, 15\n")
		fmt.Printf("  jnz .Lcall%d\n", seq)
		fmt.Printf("  mov rax, 0\n")
		fmt.Printf("  call %s\n", node.funcname)
		fmt.Printf("  jmp .Lend%d\n", seq)
		fmt.Printf(".Lcall%d:\n", seq)
		fmt.Printf("  sub rsp, 8\n")
		fmt.Printf("  mov rax, 0\n")
		fmt.Printf("  call %s\n", node.funcname)
		fmt.Printf("  add rsp, 8\n")
		fmt.Printf(".Lend%d:\n", seq)
		fmt.Printf("  push rax\n")

		return
	case ND_RETURN:
		gen(node.lhs)
		fmt.Printf("  pop rax\n")
		fmt.Printf("  jmp .Lreturn.%s\n", funcname)
		return
	}

	gen(node.lhs)
	gen(node.rhs)

	fmt.Printf("  pop rdi\n")
	fmt.Printf("  pop rax\n")

	switch node.kind {
	case ND_ADD:
		if node.ty.base != nil {
			fmt.Printf("  imul rdi, %d\n", sizeOf(node.ty.base))
		}
		fmt.Printf("  add rax, rdi\n")
	case ND_SUB:
		if node.ty.base != nil {
			fmt.Printf("  imul rdi, %d\n", sizeOf(node.ty.base))
		}
		fmt.Printf("  sub rax, rdi\n")
	case ND_MUL:
		fmt.Printf("  imul rax, rdi\n")
	case ND_DIV:
		fmt.Printf("  cqo\n")
		fmt.Printf("  idiv rdi\n")
	case ND_EQ:
		fmt.Printf("  cmp rax, rdi\n")
		fmt.Printf("  sete al\n")
		fmt.Printf("  movzb rax, al\n")
	case ND_NE:
		fmt.Printf("  cmp rax, rdi\n")
		fmt.Printf("  setne al\n")
		fmt.Printf("  movzb rax, al\n")
	case ND_LT:
		fmt.Printf("  cmp rax, rdi\n")
		fmt.Printf("  setl al\n")
		fmt.Printf("  movzb rax, al\n")
	case ND_LE:
		fmt.Printf("  cmp rax, rdi\n")
		fmt.Printf("  setle al\n")
		fmt.Printf("  movzb rax, al\n")
	}

	fmt.Printf("  push rax\n")
}

func emitData(prog *Program) {
	fmt.Printf(".data\n")

	for vl := prog.globals; vl != nil; vl = vl.next {
		v := vl.variable
		fmt.Printf("%s:\n", v.name)

		if v.contents == "" {
			fmt.Printf("  .zero %d\n", sizeOf(v.ty))
			continue
		}

		for i := 0; i < v.contLen; i++ {
			fmt.Printf("  .byte %d\n", v.contents[i])
		}
	}
}

func loadArg(v *Variable, idx int) {
	sz := sizeOf(v.ty)
	switch sz {
	case 1:
		fmt.Printf("  mov [rbp-%d], %s\n", v.offset, argreg1[idx])
	case 2:
		fmt.Printf("  mov [rbp-%d], %s\n", v.offset, argreg2[idx])
	case 4:
		fmt.Printf("  mov [rbp-%d], %s\n", v.offset, argreg4[idx])
	default:
		if sz != 8 {
			panic("sz != 8")
		}
		fmt.Printf("  mov [rbp-%d], %s\n", v.offset, argreg8[idx])
	}
}

func emitText(prog *Program) {
	fmt.Printf(".text\n")

	for fn := prog.fns; fn != nil; fn = fn.next {
		fmt.Printf(".global %s\n", fn.name)
		fmt.Printf("%s:\n", fn.name)

		funcname = fn.name

		// Prologue
		fmt.Printf("  push rbp\n")
		fmt.Printf("  mov rbp, rsp\n")
		fmt.Printf("  sub rsp, %d\n", fn.stackSize)

		// Push arguments to the stack
		i := 0
		for vl := fn.params; vl != nil; vl = vl.next {
			loadArg(vl.variable, i)
			i++
		}

		// Emit code
		for node := fn.node; node != nil; node = node.next {
			gen(node)
		}

		// Epilogue
		fmt.Printf(".Lreturn.%s:\n", funcname)
		fmt.Printf("  mov rsp, rbp\n")
		fmt.Printf("  pop rbp\n")
		fmt.Printf("  ret\n")
	}
}

func codegen(prog *Program) {
	fmt.Printf(".intel_syntax noprefix\n")
	emitData(prog)
	emitText(prog)
}

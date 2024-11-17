package main

import "fmt"

var argreg = []string{"rdi", "rsi", "rdx", "rcx", "r8", "r9"}

var labelseq int
var funcname string

// Pushes the given node's address to the stack.
func genAddr(node *Node) {
	switch node.kind {
	case ND_VAR:
		fmt.Printf("  lea rax, [rbp-%d]\n", node.variable.offset)
		fmt.Printf("  push rax\n")
		return
	case ND_DEREF:
		gen(node.lhs)
		return
	}

	errorTok(node.tok, "not an lvalue")
}

func load() {
	fmt.Printf("  pop rax\n")
	fmt.Printf("  mov rax, [rax]\n")
	fmt.Printf("  push rax\n")
}

func store() {
	fmt.Printf("  pop rdi\n")
	fmt.Printf("  pop rax\n")
	fmt.Printf("  mov [rax], rdi\n")
	fmt.Printf("  push rdi\n")
}

// Generate code for a given node.
func gen(node *Node) {
	switch node.kind {
	case ND_NULL:
		return
	case ND_NUM:
		fmt.Printf("  push %d\n", node.val)
		return
	case ND_EXPR_STMT:
		gen(node.lhs)
		fmt.Printf("  add rsp, 8\n")
		return
	case ND_VAR:
		genAddr(node)
		load()
		return
	case ND_ASSIGN:
		genAddr(node.lhs)
		gen(node.rhs)
		store()
		return
	case ND_ADDR:
		genAddr(node.lhs)
		return
	case ND_DEREF:
		gen(node.lhs)
		load()
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
	case ND_BLOCK:
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
			fmt.Printf("  pop %s\n", argreg[i])
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
		if node.ty.kind == TY_PTR {
			fmt.Printf("  imul rdi, 8\n")
		}
		fmt.Printf("  add rax, rdi\n")
	case ND_SUB:
		if node.ty.kind == TY_PTR {
			fmt.Printf("  imul rdi, 8\n")
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

func codegen(prog *Function) {
	fmt.Printf(".intel_syntax noprefix\n")

	for fn := prog; fn != nil; fn = fn.next {
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
			v := vl.variable
			fmt.Printf("  mov [rbp-%d], %s\n", v.offset, argreg[i])
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

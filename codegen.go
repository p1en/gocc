package main

import "fmt"

// Pushes the given node's address to the stack.
func genAddr(node *Node) {
	if node.kind == ND_VAR {
		fmt.Printf("  lea rax, [rbp-%d]\n", node.variable.offset)
		fmt.Printf("  push rax\n")
		return
	}

	reportError("not an lvalue")
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
	case ND_RETURN:
		gen(node.lhs)
		fmt.Printf("  pop rax\n")
		fmt.Printf("  jmp .Lreturn\n")
		return
	}

	gen(node.lhs)
	gen(node.rhs)

	fmt.Printf("  pop rdi\n")
	fmt.Printf("  pop rax\n")

	switch node.kind {
	case ND_ADD:
		fmt.Printf("  add rax, rdi\n")
	case ND_SUB:
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

func codegen(prog *Program) {
	fmt.Printf(".intel_syntax noprefix\n")
	fmt.Printf(".global main\n")
	fmt.Printf("main:\n")

	// Prologue
	fmt.Printf("  push rbp\n")
	fmt.Printf("  mov rbp, rsp\n")
	fmt.Printf("  sub rsp, %d\n", prog.stackSize)

	// Emit code
	for node := prog.node; node != nil; node = node.next {
		gen(node)
	}

	// Epilogue
	fmt.Printf(".Lreturn:\n")
	fmt.Printf("  mov rsp, rbp\n")
	fmt.Printf("  pop rbp\n")
	fmt.Printf("  ret\n")
}

package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
	"unicode"
)

//
// Tokenizer
//

// Token kind
type TokenKind int

const (
	TK_RESERVED TokenKind = iota // Keywords or punctuators
	TK_NUM                       // Integer literals
	TK_EOF                       // End-of-file markers
)

// Token type
type Token struct {
	kind TokenKind // Token kind
	next *Token    // Next token
	val  int       // If kind is TK_NUM, its value
	str  string    // Token string
	len  int       // Token length
}

// Input program
var userInput string

// Current token
var token *Token

// Returns the first numerical value found and its number of characters.
func getNum(str string) (int, int) {
	var s string
	var cnt int

	for _, c := range str {
		if unicode.IsDigit(c) {
			s += string(c)
			cnt++
		} else {
			break
		}
	}

	result, _ := strconv.Atoi(s)

	return result, cnt
}

// Reports an error and exit.
func reportError(format string, a ...any) {
	fmt.Fprintf(os.Stderr, format, a...)
	os.Exit(1)
}

// Reports an error location and exit.
func reportErrorAt(loc string, format string, a ...any) {
	pos := len(userInput) - len(loc)
	fmt.Fprintf(os.Stderr, "%s\n", userInput)
	fmt.Fprintf(os.Stderr, "%*s", pos, " ")
	fmt.Fprintf(os.Stderr, "^ ")
	fmt.Fprintf(os.Stderr, format, a...)
	fmt.Fprintf(os.Stderr, "\n")
	os.Exit(1)
}

// Consumes the current token if it matches `op`.
func consume(op string) bool {
	if token.kind != TK_RESERVED || len(op) != token.len || token.str[:token.len] != op {
		return false
	}

	token = token.next

	return true
}

// Ensure that the current token is `op`.
func expect(op string) {
	if token.kind != TK_RESERVED || len(op) != token.len || token.str[:token.len] != op {
		reportErrorAt(token.str, "expected '%s'", op)
	}

	token = token.next
}

// Ensure that the current token is TK_NUM.
func expectNumber() int {
	if token.kind != TK_NUM {
		reportErrorAt(token.str, "expected a number")
	}

	val := token.val
	token = token.next

	return val
}

func atEOF() bool {
	return token.kind == TK_EOF
}

// Create a new token and add it as the next token of `cur`.
func newToken(kind TokenKind, cur *Token, str string, len int) *Token {
	tok := &Token{kind: kind, str: str, len: len}
	cur.next = tok

	return tok
}

func startswith(p string, q string) bool {
	return strings.HasPrefix(p, q)
}

// Tokenize `user_input` and returns new tokens.
func tokenize() *Token {
	p := userInput
	head := &Token{}
	cur := head

	for len(p) > 0 {
		c := p[0]

		// Skip whitespace characters.
		if c == ' ' {
			p = p[1:]
			continue
		}

		// Multi-letter punctuator
		if startswith(p, "==") || startswith(p, "!=") || startswith(p, "<=") || startswith(p, ">=") {
			cur = newToken(TK_RESERVED, cur, p[:2], 2)
			p = p[2:]
			continue
		}

		// Single-letter punctuator
		if strings.ContainsRune("+-*/()<>", rune(c)) {
			cur = newToken(TK_RESERVED, cur, p[:1], 1)
			p = p[1:]
			continue
		}

		// Integer literal
		if unicode.IsDigit(rune(c)) {
			n, cnt := getNum(p)
			cur = newToken(TK_NUM, cur, p[:cnt], cnt)
			cur.val = n
			p = p[cnt:]
			continue
		}

		reportErrorAt(p, "invalid token")
	}

	newToken(TK_EOF, cur, "", 0)

	return head.next
}

//
// Parser
//

// AST node kind
type NodeKind int

const (
	ND_ADD NodeKind = iota // +
	ND_SUB                 // -
	ND_MUL                 // *
	ND_DIV                 // /
	ND_EQ                  // ==
	ND_NE                  // !=
	ND_LT                  // <
	ND_LE                  // <=
	ND_NUM                 // Integer
)

// AST node type
type Node struct {
	kind NodeKind // Node kind
	lhs  *Node    // Left-hand side
	rhs  *Node    // Right-hand side
	val  int      // Used if kind == ND_NUM
}

func newBinary(kind NodeKind, lhs *Node, rhs *Node) *Node {
	return &Node{kind: kind, lhs: lhs, rhs: rhs}
}

func newNum(val int) *Node {
	return &Node{kind: ND_NUM, val: val}
}

// expr = equality
func expr() *Node {
	return equality()
}

// equality = relational ("==" relational | "!=" relational)*
func equality() *Node {
	node := relational()

	for {
		if consume("==") {
			node = newBinary(ND_EQ, node, relational())
		} else if consume("!=") {
			node = newBinary(ND_NE, node, relational())
		} else {
			return node
		}
	}
}

// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
func relational() *Node {
	node := add()

	for {
		if consume("<") {
			node = newBinary(ND_LT, node, add())
		} else if consume("<=") {
			node = newBinary(ND_LE, node, add())
		} else if consume(">") {
			node = newBinary(ND_LT, add(), node)
		} else if consume(">=") {
			node = newBinary(ND_LE, add(), node)
		} else {
			return node
		}
	}
}

// add = mul ("+" mul | "-" mul)*
func add() *Node {
	node := mul()

	for {
		if consume("+") {
			node = newBinary(ND_ADD, node, mul())
		} else if consume("-") {
			node = newBinary(ND_SUB, node, mul())
		} else {
			return node
		}
	}
}

// mul = unary ("*" unary | "/" unary)*
func mul() *Node {
	node := unary()

	for {
		if consume("*") {
			node = newBinary(ND_MUL, node, unary())
		} else if consume("/") {
			node = newBinary(ND_DIV, node, unary())
		} else {
			return node
		}
	}
}

// unary = ("+" | "-")? unary | primary
func unary() *Node {
	if consume("+") {
		return unary()
	}

	if consume("-") {
		return newBinary(ND_SUB, newNum(0), unary())
	}

	return primary()
}

// primary = "(" expr ")" | num
func primary() *Node {
	if consume("(") {
		node := expr()
		expect(")")

		return node
	}

	return newNum(expectNumber())
}

//
// Code generator
//

func gen(node *Node) {
	if node.kind == ND_NUM {
		fmt.Printf("  push %d\n", node.val)
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

func main() {
	if len(os.Args) != 2 {
		reportError("%s: invalid number of arguments", os.Args[0])
	}

	// Tokenize and parse.
	userInput = os.Args[1]
	token = tokenize()
	node := expr()

	// Print out the first half of assembly.
	fmt.Printf(".intel_syntax noprefix\n")
	fmt.Printf(".global main\n")
	fmt.Printf("main:\n")

	// Traverse the AST to emit assembly.
	gen(node)

	// A result must be at the top of the stack, so pop it to RAX to make it a program exit code.
	fmt.Printf("  pop rax\n")
	fmt.Printf("  ret\n")
}

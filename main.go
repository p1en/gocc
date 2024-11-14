package main

import (
	"fmt"
	"os"
	"strconv"
	"unicode"
)

type TokenKind int

const (
	TK_RESERVED TokenKind = iota // Keywords or punctuators
	TK_NUM                       // Integer literals
	TK_EOF                       // End-of-file markers
)

type Token struct {
	kind TokenKind // Token kind
	next *Token    // Next token
	val  int       // If kind is TK_NUM, its value
	str  string    // Token string
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
func consume(op byte) bool {
	if token.kind != TK_RESERVED || token.str[0] != op {
		return false
	}

	token = token.next

	return true
}

// Ensure that the current token is `op`.
func expect(op byte) {
	if token.kind != TK_RESERVED || token.str[0] != op {
		reportErrorAt(token.str, "expected '%c'", op)
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
func newToken(kind TokenKind, cur *Token, str string) *Token {
	tok := &Token{kind: kind, str: str}
	cur.next = tok

	return tok
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

		// Punctuator
		if c == '+' || c == '-' {
			cur = newToken(TK_RESERVED, cur, p[:1])
			p = p[1:]
			continue
		}

		// Integer literal
		if unicode.IsDigit(rune(c)) {
			n, cnt := getNum(p)
			cur = newToken(TK_NUM, cur, p[:cnt])
			cur.val = n
			p = p[cnt:]
			continue
		}

		reportErrorAt(p, "expected a number")
	}

	newToken(TK_EOF, cur, "")

	return head.next
}

func main() {
	if len(os.Args) != 2 {
		reportError("%s: invalid number of arguments", os.Args[0])
	}

	userInput = os.Args[1]
	token = tokenize()

	fmt.Printf(".intel_syntax noprefix\n")
	fmt.Printf(".global main\n")
	fmt.Printf("main:\n")

	fmt.Printf("  mov rax, %d\n", expectNumber())

	for !atEOF() {
		if consume('+') {
			fmt.Printf("  add rax, %d\n", expectNumber())
			continue
		}

		expect('-')
		fmt.Printf("  sub rax, %d\n", expectNumber())
	}

	fmt.Printf("  ret\n")
}

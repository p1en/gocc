package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
	"unicode"
)

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

var userInput string
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

func isAlpha(c byte) bool {
	return unicode.IsLetter(rune(c)) || c == '_'
}

func isAlnum(c byte) bool {
	return isAlpha(c) || unicode.IsDigit(rune(c))
}

// Tokenize `userInput` and returns new tokens.
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

		// Keyword
		if startswith(p, "return") && !isAlnum(p[6]) {
			cur = newToken(TK_RESERVED, cur, p[:6], 6)
			p = p[6:]
			continue
		}

		// Multi-letter punctuator
		if startswith(p, "==") || startswith(p, "!=") || startswith(p, "<=") || startswith(p, ">=") {
			cur = newToken(TK_RESERVED, cur, p[:2], 2)
			p = p[2:]
			continue
		}

		// Single-letter punctuator
		if strings.ContainsRune("+-*/()<>;", rune(c)) {
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

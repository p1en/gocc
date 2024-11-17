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
	TK_IDENT                     // Identifiers
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
func verrorAt(loc string, format string, a ...any) {
	pos := len(userInput) - len(loc)
	fmt.Fprintf(os.Stderr, "%s\n", userInput)
	fmt.Fprintf(os.Stderr, "%*s", pos, " ")
	fmt.Fprintf(os.Stderr, "^ ")
	fmt.Fprintf(os.Stderr, format, a...)
	fmt.Fprintf(os.Stderr, "\n")
	os.Exit(1)
}

// Reports an error location and exit.
func errorAt(loc string, format string, a ...any) {
	verrorAt(loc, fmt.Sprintf(format, a...))
}

// Reports an error location and exit.
func errorTok(tok *Token, format string, a ...any) {
	if tok != nil {
		verrorAt(tok.str, format, a...)
	}

	fmt.Fprintf(os.Stderr, format, a...)
	fmt.Fprintln(os.Stderr)
	os.Exit(1)
}

// Returns true if the current token matches a given string.
func peek(s string) *Token {
	if token.kind != TK_RESERVED || len(s) != token.len || token.str[:token.len] != s {
		return nil
	}

	return token
}

// Consumes the current token if it matches a given string.
func consume(s string) *Token {
	if peek(s) == nil {
		return nil
	}

	t := token
	token = token.next

	return t
}

// Consumes the current token if it is an identifier.
func consumeIdent() *Token {
	if token.kind != TK_IDENT {
		return nil
	}

	t := token
	token = token.next

	return t
}

// Ensure that the current token is a given string.
func expect(s string) {
	if peek(s) == nil {
		errorTok(token, "expected \"%s\"", s)
	}

	token = token.next
}

// Ensure that the current token is TK_NUM.
func expectNumber() int {
	if token.kind != TK_NUM {
		errorTok(token, "expected a number")
	}

	val := token.val
	token = token.next

	return val
}

// Ensure that the current token is TK_IDENT.
func expectIdent() string {
	if token.kind != TK_IDENT {
		errorTok(token, "expected an identifier")
	}

	s := token.str[:token.len]
	token = token.next

	return s
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

func startsWithReserved(p string) string {
	// Keyword
	kw := []string{"return", "if", "else", "while", "for", "int", "char", "sizeof"}
	for _, v := range kw {
		l := len(v)
		if startswith(p, v) && !isAlnum(p[l]) {
			return v
		}
	}

	// Multi-letter punctuator
	ops := []string{"==", "!=", "<=", ">="}
	for _, v := range ops {
		if startswith(p, v) {
			return v
		}
	}

	return ""
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

		// Keyword or multi-letter punctuator
		kw := startsWithReserved(p)
		if kw != "" {
			l := len(kw)
			cur = newToken(TK_RESERVED, cur, p, l)
			p = p[l:]
			continue
		}

		// Single-letter punctuator
		if strings.ContainsRune("+-*/()<>;={},&[]", rune(c)) {
			cur = newToken(TK_RESERVED, cur, p, 1)
			p = p[1:]
			continue
		}

		// Identifier
		if unicode.IsLetter(rune(c)) {
			q := p
			pl := len(p)
			p = p[1:]
			for isAlnum(p[0]) && len(p) > 0 {
				p = p[1:]
			}
			cur = newToken(TK_IDENT, cur, q, pl-len(p))
			continue
		}

		// Integer literal
		if unicode.IsDigit(rune(c)) {
			cur = newToken(TK_NUM, cur, p, 0)
			n, cnt := getNum(p)
			cur.val = n
			cur.len = cnt
			p = p[cnt:]
			continue
		}

		errorAt(p, "invalid token")
	}

	newToken(TK_EOF, cur, "", 0)

	return head.next
}

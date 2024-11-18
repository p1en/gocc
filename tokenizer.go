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
	TK_STR                       // String literals
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

	contents string // String literal contents including terminating '\0'
	contLen  byte   // String literal length
}

var filename string
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

// Reports an error message in the following format and exit.
func verrorAt(loc string, format string, a ...any) {
	// Find a line containing `loc`.
	line := loc
	for strings.Index(userInput, line) > 0 && line[0] != '\n' {
		line = line[1:]
	}

	end := loc
	for end[0] != '\n' {
		end = end[1:]
	}

	// Get a line number.
	lineNum := 1
	for _, c := range userInput {
		if c == '\n' {
			lineNum++
		}
		if strings.HasPrefix(userInput, line) {
			break
		}
	}

	// Print out the line.
	indent, _ := fmt.Fprintf(os.Stderr, "%s:%d: ", filename, lineNum)
	fmt.Fprintf(os.Stderr, "%s\n", line)

	// Show the error message.
	pos := strings.Index(loc, line) + indent
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

func getEscapeChar(c byte) byte {
	switch c {
	case 'a':
		return '\a'
	case 'b':
		return '\b'
	case 't':
		return '\t'
	case 'n':
		return '\n'
	case 'v':
		return '\v'
	case 'f':
		return '\f'
	case 'r':
		return '\r'
	case 'e':
		return 27
	case '0':
		return 0
	default:
		return c
	}
}

func readStringLiteral(cur *Token, start string) *Token {
	p := start[1:]
	buf := make([]byte, 0)
	l := 0

	for {
		c := p[0]
		if l == 1024 {
			errorAt(p, "string literal too large")
		}
		if c == 0 {
			errorAt(p, "unclosed string literal")
		}
		if c == '"' {
			break
		}

		if c == '\\' {
			p = p[1:]
			buf = append(buf, getEscapeChar(p[0]))
			p = p[1:]
		} else {
			buf = append(buf, c)
			p = p[1:]
		}
	}

	tok := newToken(TK_STR, cur, start, len(start)-len(p)+1)
	tok.contents = string(append(buf, 0))
	tok.contLen = byte(len(tok.contents))

	return tok
}

// Tokenize `userInput` and returns new tokens.
func tokenize() *Token {
	p := userInput
	head := &Token{}
	cur := head

	for len(p) > 0 {
		c := p[0]

		// Skip whitespace characters.
		if unicode.IsSpace(rune(c)) {
			p = p[1:]
			continue
		}

		// Skip line comments.
		if startswith(p, "//") {
			p = p[2:]
			for p[0] != '\n' {
				p = p[1:]
			}
			continue
		}

		// Skip block comments.
		if startswith(p, "/*") {
			for len(p) > 1 && !startswith(p, "*/") {
				p = p[1:]
			}
			if len(p) < 2 {
				errorAt(p, "unclosed block comment")
			}
			p = p[2:]
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

		// String literal
		if c == '"' {
			cur = readStringLiteral(cur, p)
			p = p[cur.len:]

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

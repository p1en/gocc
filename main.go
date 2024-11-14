package main

import (
	"fmt"
	"os"
	"strconv"
	"unicode"
)

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

func main() {
	if len(os.Args) != 2 {
		fmt.Fprintf(os.Stderr, "%s: invalid number of arguments\n", os.Args[0])
		os.Exit(1)
	}

	p := os.Args[1]

	fmt.Printf(".intel_syntax noprefix\n")
	fmt.Printf(".global main\n")
	fmt.Printf("main:\n")

	n, cnt := getNum(p)
	fmt.Printf("  mov rax, %d\n", n)
	p = p[cnt:]

	for len(p) > 0 {
		switch p[0] {
		case '+':
			p = p[1:]
			n, cnt := getNum(p)
			fmt.Printf("  add rax, %d\n", n)
			p = p[cnt:]
		case '-':
			p = p[1:]
			n, cnt := getNum(p)
			fmt.Printf("  sub rax, %d\n", n)
			p = p[cnt:]
		default:
			fmt.Fprintf(os.Stderr, "unexpected character: %q\n", p[0])
			os.Exit(1)
		}
	}

	fmt.Printf("  ret\n")
}

package main

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
	next *Node    // Next node
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

// program = stmt*
func program() *Node {
	head := &Node{}
	cur := head

	for !atEOF() {
		cur.next = stmt()
		cur = cur.next
	}

	return head.next
}

// stmt = expr ";"
func stmt() *Node {
	node := expr()
	expect(";")

	return node
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

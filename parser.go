package main

type Variable struct {
	name   string // Variable name
	offset int    // Offset from RBP
}

type VariableList struct {
	next     *VariableList
	variable *Variable
}

// AST node
type NodeKind int

const (
	ND_ADD       NodeKind = iota // +
	ND_SUB                       // -
	ND_MUL                       // *
	ND_DIV                       // /
	ND_EQ                        // ==
	ND_NE                        // !=
	ND_LT                        // <
	ND_LE                        // <=
	ND_ASSIGN                    // =
	ND_RETURN                    // "return"
	ND_IF                        // "if"
	ND_WHILE                     // "while"
	ND_FOR                       // "for"
	ND_BLOCK                     // { ... }
	ND_FUNCALL                   // Function call
	ND_EXPR_STMT                 // Expression statement
	ND_VAR                       // Variable
	ND_NUM                       // Integer
)

// AST node type
type Node struct {
	kind NodeKind // Node kind
	next *Node    // Next node
	lhs  *Node    // Left-hand side
	rhs  *Node    // Right-hand side

	// "if, "while" or "for" statement
	cond *Node
	then *Node
	els  *Node
	init *Node
	inc  *Node

	// Block
	body *Node

	// Function call
	funcname string
	args     *Node

	variable *Variable // Used if kind == ND_VAR
	val      int       // Used if kind == ND_NUM
}

type Function struct {
	next   *Function
	name   string
	params *VariableList

	node      *Node
	locals    *VariableList
	stackSize int
}

var locals *VariableList

// Find a local variable by name.
func findVar(tok *Token) *Variable {
	for vl := locals; vl != nil; vl = vl.next {
		v := vl.variable
		if len(v.name) == tok.len && tok.str[:tok.len] == v.name {
			return v
		}
	}

	return nil
}

func newNode(kind NodeKind) *Node {
	return &Node{kind: kind}
}

func newBinary(kind NodeKind, lhs *Node, rhs *Node) *Node {
	return &Node{kind: kind, lhs: lhs, rhs: rhs}
}

func newUnary(kind NodeKind, expr *Node) *Node {
	return &Node{kind: kind, lhs: expr}
}

func newNum(val int) *Node {
	return &Node{kind: ND_NUM, val: val}
}

func newVar(variable *Variable) *Node {
	return &Node{kind: ND_VAR, variable: variable}
}

func pushVar(name string) *Variable {
	v := &Variable{name: name}
	vl := &VariableList{variable: v, next: locals}
	locals = vl

	return v
}

// program = function*
func program() *Function {
	head := &Function{}
	cur := head

	for !atEOF() {
		cur.next = function()
		cur = cur.next
	}

	return head.next
}

func readFuncParams() *VariableList {
	if consume(")") {
		return nil
	}

	head := &VariableList{variable: pushVar(expectIdent())}
	cur := head

	for !consume(")") {
		expect(",")
		cur.next = &VariableList{variable: pushVar(expectIdent())}
		cur = cur.next
	}

	return head
}

// function = ident "(" params? ")" "{" stmt* "}"
// params   = ident ("," ident)*
func function() *Function {
	fn := &Function{name: expectIdent()}
	expect("(")
	fn.params = readFuncParams()
	expect("{")

	head := &Node{}
	cur := head
	for !consume("}") {
		cur.next = stmt()
		cur = cur.next
	}

	fn.node = head.next
	fn.locals = locals

	return fn
}

func readExprStmt() *Node {
	return newUnary(ND_EXPR_STMT, expr())
}

// stmt = "return" expr ";"
//
//	| "if" "(" expr ")" stmt ("else" stmt)?
//	| "while" "(" expr ")" stmt
//	| "for" "(" expr? ";" expr? ";" expr? ")" stmt
//	| "{" stmt* "}"
//	| expr ";"
func stmt() *Node {
	if consume("return") {
		node := newUnary(ND_RETURN, expr())
		expect(";")

		return node
	}

	if consume("if") {
		node := newNode(ND_IF)
		expect("(")
		node.cond = expr()
		expect(")")
		node.then = stmt()

		if consume("else") {
			node.els = stmt()
		}

		return node
	}

	if consume("while") {
		node := newNode(ND_WHILE)
		expect("(")
		node.cond = expr()
		expect(")")
		node.then = stmt()

		return node
	}

	if consume("for") {
		node := newNode(ND_FOR)
		expect("(")
		if !consume(";") {
			node.init = readExprStmt()
			expect(";")
		}
		if !consume(";") {
			node.cond = expr()
			expect(";")
		}
		if !consume(")") {
			node.inc = readExprStmt()
			expect(")")
		}
		node.then = stmt()

		return node
	}

	if consume("{") {
		head := &Node{}
		cur := head

		for !consume("}") {
			cur.next = stmt()
			cur = cur.next
		}

		return &Node{kind: ND_BLOCK, body: head.next}
	}

	node := readExprStmt()
	expect(";")

	return node
}

// expr = assign
func expr() *Node {
	return assign()
}

// assign = equality ("=" assign)?
func assign() *Node {
	node := equality()

	if consume("=") {
		node = newBinary(ND_ASSIGN, node, assign())
	}

	return node
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

// unary = ("+" | "-")? unary
//
//	| primary
func unary() *Node {
	if consume("+") {
		return unary()
	}

	if consume("-") {
		return newBinary(ND_SUB, newNum(0), unary())
	}

	return primary()
}

// func-args = "(" (assign ("," assign)*)? ")"
func funcArgs() *Node {
	if consume(")") {
		return nil
	}

	head := assign()
	cur := head
	for consume(",") {
		cur.next = assign()
		cur = cur.next
	}
	expect(")")

	return head
}

// primary = "(" expr ")" | ident func-args? | num
func primary() *Node {
	if consume("(") {
		node := expr()
		expect(")")

		return node
	}

	tok := consumeIdent()
	if tok != nil {
		if consume("(") {
			return &Node{kind: ND_FUNCALL, funcname: tok.str[:tok.len], args: funcArgs()}
		}

		variable := findVar(tok)
		if variable == nil {
			variable = pushVar(tok.str[:tok.len])
		}
		return newVar(variable)
	}

	return newNum(expectNumber())
}

package main

type Variable struct {
	name   string // Variable name
	ty     *Type  // Type
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
	ND_ADDR                      // unary &
	ND_DEREF                     // unary *
	ND_RETURN                    // "return"
	ND_IF                        // "if"
	ND_WHILE                     // "while"
	ND_FOR                       // "for"
	ND_SIZEOF                    // "sizeof"
	ND_BLOCK                     // { ... }
	ND_FUNCALL                   // Function call
	ND_EXPR_STMT                 // Expression statement
	ND_VAR                       // Variable
	ND_NUM                       // Integer
	ND_NULL                      // Empty statement
)

// AST node type
type Node struct {
	kind NodeKind // Node kind
	next *Node    // Next node
	ty   *Type    // Type, e.g. int or pointer to int
	tok  *Token   // Representative token

	lhs *Node // Left-hand side
	rhs *Node // Right-hand side

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

func newNode(kind NodeKind, tok *Token) *Node {
	return &Node{kind: kind, tok: tok}
}

func newBinary(kind NodeKind, lhs *Node, rhs *Node, tok *Token) *Node {
	return &Node{kind: kind, lhs: lhs, rhs: rhs, tok: tok}
}

func newUnary(kind NodeKind, expr *Node, tok *Token) *Node {
	return &Node{kind: kind, lhs: expr, tok: tok}
}

func newNum(val int, tok *Token) *Node {
	return &Node{kind: ND_NUM, val: val, tok: tok}
}

func newVar(variable *Variable, tok *Token) *Node {
	return &Node{kind: ND_VAR, variable: variable, tok: tok}
}

func pushVar(name string, ty *Type) *Variable {
	v := &Variable{name: name, ty: ty}
	vl := &VariableList{variable: v, next: locals}
	locals = vl

	return v
}

// program = function*
func program() *Function {
	head := Function{}
	cur := &head

	for !atEOF() {
		cur.next = function()
		cur = cur.next
	}

	return head.next
}

// basetype = "int" "*"*
func baseType() *Type {
	expect("int")
	ty := intType()
	for consume("*") != nil {
		ty = pointerTo(ty)
	}

	return ty
}

func readTypeSuffix(base *Type) *Type {
	if consume("[") == nil {
		return base
	}

	sz := expectNumber()
	expect("]")
	base = readTypeSuffix(base)

	return arrayOf(base, sz)
}

func readFuncParam() *VariableList {
	ty := baseType()
	name := expectIdent()
	ty = readTypeSuffix(ty)

	return &VariableList{variable: pushVar(name, ty)}
}

func readFuncParams() *VariableList {
	if consume(")") != nil {
		return nil
	}

	head := readFuncParam()
	cur := head

	for consume(")") == nil {
		expect(",")
		cur.next = readFuncParam()
		cur = cur.next
	}

	return head
}

// function = basetype ident "(" params? ")" "{" stmt* "}"
// params   = param ("," param)*
// param    = basetype ident
func function() *Function {
	locals = nil

	fn := &Function{}
	baseType()
	fn.name = expectIdent()
	expect("(")
	fn.params = readFuncParams()
	expect("{")

	head := &Node{}
	cur := head
	for consume("}") == nil {
		cur.next = stmt()
		cur = cur.next
	}

	fn.node = head.next
	fn.locals = locals

	return fn
}

// declaration = basetype ident ("[" num "]")* ("=" expr) ";"
func declaration() *Node {
	tok := token
	ty := baseType()
	name := expectIdent()
	ty = readTypeSuffix(ty)
	v := pushVar(name, ty)

	if consume(";") != nil {
		return newNode(ND_NULL, tok)
	}

	expect("=")
	lhs := newVar(v, tok)
	rhs := expr()
	expect(";")
	node := newBinary(ND_ASSIGN, lhs, rhs, tok)

	return newUnary(ND_EXPR_STMT, node, tok)
}

func readExprStmt() *Node {
	tok := token
	return newUnary(ND_EXPR_STMT, expr(), tok)
}

// stmt = "return" expr ";"
//
//	| "if" "(" expr ")" stmt ("else" stmt)?
//	| "while" "(" expr ")" stmt
//	| "for" "(" expr? ";" expr? ";" expr? ")" stmt
//	| "{" stmt* "}"
//	| declaration
//	| expr ";"
func stmt() *Node {
	var tok *Token

	if tok = consume("return"); tok != nil {
		node := newUnary(ND_RETURN, expr(), tok)
		expect(";")

		return node
	}

	if tok = consume("if"); tok != nil {
		node := newNode(ND_IF, tok)
		expect("(")
		node.cond = expr()
		expect(")")
		node.then = stmt()

		if consume("else") != nil {
			node.els = stmt()
		}

		return node
	}

	if tok = consume("while"); tok != nil {
		node := newNode(ND_WHILE, tok)
		expect("(")
		node.cond = expr()
		expect(")")
		node.then = stmt()

		return node
	}

	if tok = consume("for"); tok != nil {
		node := newNode(ND_FOR, tok)
		expect("(")
		if consume(";") == nil {
			node.init = readExprStmt()
			expect(";")
		}
		if consume(";") == nil {
			node.cond = expr()
			expect(";")
		}
		if consume(")") == nil {
			node.inc = readExprStmt()
			expect(")")
		}
		node.then = stmt()

		return node
	}

	if tok = consume("{"); tok != nil {
		head := &Node{}
		cur := head

		for consume("}") == nil {
			cur.next = stmt()
			cur = cur.next
		}

		node := newNode(ND_BLOCK, tok)
		node.body = head.next

		return node
	}

	if tok = peek("int"); tok != nil {
		return declaration()
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
	var tok *Token

	if tok = consume("="); tok != nil {
		node = newBinary(ND_ASSIGN, node, assign(), tok)
	}

	return node
}

// equality = relational ("==" relational | "!=" relational)*
func equality() *Node {
	node := relational()
	var tok *Token

	for {
		if tok = consume("=="); tok != nil {
			node = newBinary(ND_EQ, node, relational(), tok)
		} else if tok = consume("!="); tok != nil {
			node = newBinary(ND_NE, node, relational(), tok)
		} else {
			return node
		}
	}
}

// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
func relational() *Node {
	node := add()
	var tok *Token

	for {
		if tok = consume("<"); tok != nil {
			node = newBinary(ND_LT, node, add(), tok)
		} else if tok = consume("<="); tok != nil {
			node = newBinary(ND_LE, node, add(), tok)
		} else if tok = consume(">"); tok != nil {
			node = newBinary(ND_LT, add(), node, tok)
		} else if tok = consume(">="); tok != nil {
			node = newBinary(ND_LE, add(), node, tok)
		} else {
			return node
		}
	}
}

// add = mul ("+" mul | "-" mul)*
func add() *Node {
	node := mul()
	var tok *Token

	for {
		if tok = consume("+"); tok != nil {
			node = newBinary(ND_ADD, node, mul(), tok)
		} else if tok = consume("-"); tok != nil {
			node = newBinary(ND_SUB, node, mul(), tok)
		} else {
			return node
		}
	}
}

// mul = unary ("*" unary | "/" unary)*
func mul() *Node {
	node := unary()
	var tok *Token

	for {
		if tok = consume("*"); tok != nil {
			node = newBinary(ND_MUL, node, unary(), tok)
		} else if tok = consume("/"); tok != nil {
			node = newBinary(ND_DIV, node, unary(), tok)
		} else {
			return node
		}
	}
}

// unary = ("+" | "-" | "*" | "&")? unary
//
//	| postfix
func unary() *Node {
	var tok *Token

	if consume("+") != nil {
		return unary()
	}
	if tok = consume("-"); tok != nil {
		return newBinary(ND_SUB, newNum(0, tok), unary(), tok)
	}
	if tok = consume("&"); tok != nil {
		return newUnary(ND_ADDR, unary(), tok)
	}
	if tok = consume("*"); tok != nil {
		return newUnary(ND_DEREF, unary(), tok)
	}

	return postfix()
}

// postfix = primary ("[" expr "]")*
func postfix() *Node {
	node := primary()

	for tok := consume("["); tok != nil; tok = consume("[") {
		// x[y] is short for *(x+y)
		exp := newBinary(ND_ADD, node, expr(), tok)
		expect("]")
		node = newUnary(ND_DEREF, exp, tok)
	}

	return node
}

// func-args = "(" (assign ("," assign)*)? ")"
func funcArgs() *Node {
	if consume(")") != nil {
		return nil
	}

	head := assign()
	cur := head
	for consume(",") != nil {
		cur.next = assign()
		cur = cur.next
	}
	expect(")")

	return head
}

// primary = "(" expr ")" | "sizeof" unary | ident func-args? | num
func primary() *Node {
	var tok *Token

	if consume("(") != nil {
		node := expr()
		expect(")")

		return node
	}

	if tok = consume("sizeof"); tok != nil {
		return newUnary(ND_SIZEOF, unary(), tok)
	}

	if tok = consumeIdent(); tok != nil {
		if consume("(") != nil {
			node := newNode(ND_FUNCALL, tok)
			node.funcname = tok.str[:tok.len]
			node.args = funcArgs()

			return node
		}

		variable := findVar(tok)
		if variable == nil {
			errorTok(tok, "undefined variable")
		}
		return newVar(variable, tok)
	}

	tok = token
	if tok.kind != TK_NUM {
		errorTok(tok, "expected expression")
	}

	return newNum(expectNumber(), tok)
}

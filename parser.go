package main

import "strconv"

// Scope for struct tags
type TagScope struct {
	next *TagScope
	name string
	ty   *Type
}

// Variable
type Variable struct {
	name    string // Variable name
	ty      *Type  // Type
	isLocal bool   // local or global

	// Local variable
	offset int // Offset from RBP

	// Global variable
	contents string
	contLen  int
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
	ND_MEMBER                    // . (struct member access)
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
	ND_STMT_EXPR                 // Statement expression
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

	// Block or statement expression
	body *Node

	// Struct member access
	memberName string
	member     *Member

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

type Program struct {
	globals *VariableList
	fns     *Function
}

var locals *VariableList
var globals *VariableList
var scope *VariableList
var tagScope *TagScope
var labelcnt int

// Find a variable by name.
func findVar(tok *Token) *Variable {
	for vl := scope; vl != nil; vl = vl.next {
		v := vl.variable
		if len(v.name) == tok.len && tok.str[:tok.len] == v.name {
			return v
		}
	}

	return nil
}

func findTag(tok *Token) *TagScope {
	for sc := tagScope; sc != nil; sc = sc.next {
		if len(sc.name) == tok.len && tok.str[:tok.len] == sc.name {
			return sc
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

func pushVar(name string, ty *Type, isLocal bool) *Variable {
	v := &Variable{name: name, ty: ty, isLocal: isLocal}
	vl := &VariableList{variable: v}

	if isLocal {
		vl.next = locals
		locals = vl
	} else {
		vl.next = globals
		globals = vl
	}

	sc := &VariableList{variable: v, next: scope}
	scope = sc

	return v
}

func newLabel() string {
	label := ".L.data." + strconv.Itoa(labelcnt)
	labelcnt++

	return label
}

func isFunction() bool {
	tok := token
	baseType()
	isFunc := (consumeIdent() != nil) && (consume("(") != nil)
	token = tok

	return isFunc
}

// program = (global-var | function)*
func program() *Program {
	head := Function{}
	cur := &head
	globals = nil

	for !atEOF() {
		if isFunction() {
			cur.next = function()
			cur = cur.next
		} else {
			globalVar()
		}
	}

	return &Program{globals: globals, fns: head.next}
}

// basetype = ("char" | "int" | struct-decl) "*"*
func baseType() *Type {
	if !isTypename() {
		errorTok(token, "typename expected")
	}

	var ty *Type

	if consume("char") != nil {
		ty = charType()
	} else if consume("int") != nil {
		ty = intType()
	} else {
		ty = structDecl()
	}

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

func pushTagScope(tok *Token, ty *Type) {
	sc := &TagScope{next: tagScope, name: tok.str[:tok.len], ty: ty}
	tagScope = sc
}

// struct-decl = "struct" ident
//
//	| "struct" ident? "{" struct-member "}"
func structDecl() *Type {
	expect("struct")

	// Read a struct tag.
	tag := consumeIdent()
	if tag != nil && peek("{") == nil {
		sc := findTag(tag)
		if sc == nil {
			errorTok(tag, "unknown struct type")
		}

		return sc.ty
	}

	expect("{")

	// Read struct members.
	head := Member{}
	cur := &head

	for consume("}") == nil {
		cur.next = structMember()
		cur = cur.next
	}

	ty := &Type{kind: TY_STRUCT, members: head.next}

	// Assign offsets within the struct to members.
	offset := 0
	for mem := ty.members; mem != nil; mem = mem.next {
		offset = alignTo(offset, mem.ty.align)
		mem.offset = offset
		offset += sizeOf(mem.ty)

		if ty.align < mem.ty.align {
			ty.align = mem.ty.align
		}
	}

	// Register the struct type if a name was given.
	if tag != nil {
		pushTagScope(tag, ty)
	}

	return ty
}

// struct-member = basetype ident ("[" num "]")* ";"
func structMember() *Member {
	mem := &Member{ty: baseType(), name: expectIdent()}
	mem.ty = readTypeSuffix(mem.ty)
	expect(";")

	return mem
}

func readFuncParam() *VariableList {
	ty := baseType()
	name := expectIdent()
	ty = readTypeSuffix(ty)

	return &VariableList{variable: pushVar(name, ty, true)}
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

// global-var = basetype ident ("[" num "]")* ";"
func globalVar() {
	ty := baseType()
	name := expectIdent()
	ty = readTypeSuffix(ty)
	expect(";")
	pushVar(name, ty, false)
}

// declaration = basetype ident ("[" num "]")* ("=" expr) ";"
//
//	| basetype ";"
func declaration() *Node {
	tok := token
	ty := baseType()

	if consume(";") != nil {
		return newNode(ND_NULL, tok)
	}

	name := expectIdent()
	ty = readTypeSuffix(ty)
	v := pushVar(name, ty, true)

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

func isTypename() bool {
	return peek("char") != nil || peek("int") != nil || peek("struct") != nil
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

		sc1 := scope
		sc2 := tagScope
		for consume("}") == nil {
			cur.next = stmt()
			cur = cur.next
		}
		scope = sc1
		tagScope = sc2

		node := newNode(ND_BLOCK, tok)
		node.body = head.next

		return node
	}

	if isTypename() {
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

// postfix = primary ("[" expr "]" | "." ident | "->" ident)*
func postfix() *Node {
	node := primary()
	tok := &Token{}

	for {
		if tok = consume("["); tok != nil {
			// x[y] is short for *(x+y)
			exp := newBinary(ND_ADD, node, expr(), tok)
			expect("]")
			node = newUnary(ND_DEREF, exp, tok)
			continue
		}

		if tok = consume("."); tok != nil {
			node = newUnary(ND_MEMBER, node, tok)
			node.memberName = expectIdent()
			continue
		}

		if tok = consume("->"); tok != nil {
			// x->y is short for (*x).y
			node = newUnary(ND_DEREF, node, tok)
			node = newUnary(ND_MEMBER, node, tok)
			node.memberName = expectIdent()
			continue
		}

		return node
	}
}

// stmt-expr = "(" "{" stmt stmt* "}" ")"
//
// Statement expression is a GNU C extension.
func stmtExpr(tok *Token) *Node {
	sc1 := scope
	sc2 := tagScope

	node := newNode(ND_STMT_EXPR, tok)
	node.body = stmt()
	cur := node.body

	for consume("}") == nil {
		cur.next = stmt()
		cur = cur.next
	}
	expect(")")

	scope = sc1
	tagScope = sc2

	if cur.kind != ND_EXPR_STMT {
		errorTok(cur.tok, "stmt expr returning void is not supported")
	}
	*cur = *cur.lhs

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

// primary = "(" "{" stmt-expr-tail
//
//	| "(" expr ")"
//	| "sizeof" unary
//	| ident func-args?
//	| str
//	| num
func primary() *Node {
	var tok *Token

	if tok = consume("("); tok != nil {
		if consume("{") != nil {
			return stmtExpr(tok)
		}

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

	if tok.kind == TK_STR {
		token = token.next

		ty := arrayOf(charType(), int(tok.contLen))
		v := pushVar(newLabel(), ty, false)
		v.contents = tok.contents
		v.contLen = int(tok.contLen)

		return newVar(v, tok)
	}

	if tok.kind != TK_NUM {
		errorTok(tok, "expected expression")
	}

	return newNum(expectNumber(), tok)
}

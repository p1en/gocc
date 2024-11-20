package main

type TypeKind int

const (
	TY_CHAR TypeKind = iota
	TY_INT
	TY_PTR
	TY_ARRAY
	TY_STRUCT
)

type Type struct {
	kind      TypeKind
	align     int     // alignment
	base      *Type   // pointer or array
	arraySize int     // array
	members   *Member // struct
}

// Struct member
type Member struct {
	next   *Member
	ty     *Type
	name   string
	offset int
}

func alignTo(n int, align int) int {
	return (n + align - 1) & ^(align - 1)
}

func newType(kind TypeKind, align int) *Type {
	return &Type{kind: kind, align: align}
}

func charType() *Type {
	return newType(TY_CHAR, 1)
}

func intType() *Type {
	return newType(TY_INT, 4)
}

func pointerTo(base *Type) *Type {
	ty := newType(TY_PTR, 8)
	ty.base = base

	return ty
}

func arrayOf(base *Type, size int) *Type {
	ty := newType(TY_ARRAY, base.align)
	ty.base = base
	ty.arraySize = size

	return ty
}

func sizeOf(ty *Type) int {
	switch ty.kind {
	case TY_CHAR:
		return 1
	case TY_INT:
		return 4
	case TY_PTR:
		return 8
	case TY_ARRAY:
		return sizeOf(ty.base) * ty.arraySize
	default:
		if ty.kind != TY_STRUCT {
			panic("ty.kind != TY_STRUCT")
		}

		mem := ty.members
		for mem.next != nil {
			mem = mem.next
		}

		end := mem.offset + sizeOf(mem.ty)

		return alignTo(end, ty.align)
	}
}

func findMember(ty *Type, name string) *Member {
	if ty.kind != TY_STRUCT {
		panic("ty.kind != TY_STRUCT")
	}

	for mem := ty.members; mem != nil; mem = mem.next {
		if mem.name == name {
			return mem
		}
	}

	return nil
}

func visit(node *Node) {
	if node == nil {
		return
	}

	visit(node.lhs)
	visit(node.rhs)
	visit(node.cond)
	visit(node.then)
	visit(node.els)
	visit(node.init)
	visit(node.inc)

	for n := node.body; n != nil; n = n.next {
		visit(n)
	}
	for n := node.args; n != nil; n = n.next {
		visit(n)
	}

	switch node.kind {
	case ND_MUL, ND_DIV, ND_EQ, ND_NE, ND_LT, ND_LE, ND_FUNCALL, ND_NUM:
		node.ty = intType()
		return
	case ND_VAR:
		node.ty = node.variable.ty
		return
	case ND_ADD:
		if node.rhs.ty.base != nil {
			tmp := node.lhs
			node.lhs = node.rhs
			node.rhs = tmp
		}
		if node.rhs.ty.base != nil {
			errorTok(node.tok, "invalid pointer arithmetic operands")
		}
		node.ty = node.lhs.ty
		return
	case ND_SUB:
		if node.rhs.ty.base != nil {
			errorTok(node.tok, "invalid pointer arithmetic operands")
		}
		node.ty = node.lhs.ty
		return
	case ND_ASSIGN:
		node.ty = node.lhs.ty
		return
	case ND_MEMBER:
		if node.lhs.ty.kind != TY_STRUCT {
			errorTok(node.tok, "not a struct")
		}
		node.member = findMember(node.lhs.ty, node.memberName)
		if node.member == nil {
			errorTok(node.tok, "specified member does not exist")
		}
		node.ty = node.member.ty
		return
	case ND_ADDR:
		if node.lhs.ty.kind == TY_ARRAY {
			node.ty = pointerTo(node.lhs.ty.base)
		} else {
			node.ty = pointerTo(node.lhs.ty)
		}
		return
	case ND_DEREF:
		if node.lhs.ty.base == nil {
			errorTok(node.tok, "invalid pointer dereference")
		}
		node.ty = node.lhs.ty.base
		return
	case ND_SIZEOF:
		node.kind = ND_NUM
		node.ty = intType()
		node.val = sizeOf(node.lhs.ty)
		node.lhs = nil
		return
	case ND_STMT_EXPR:
		last := node.body
		for last.next != nil {
			last = last.next
		}
		node.ty = last.ty
		return
	}
}

func addType(prog *Program) {
	for fn := prog.fns; fn != nil; fn = fn.next {
		for node := fn.node; node != nil; node = node.next {
			visit(node)
		}
	}
}

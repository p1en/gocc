package main

type TypeKind int

const (
	TY_INT TypeKind = iota
	TY_PTR
)

type Type struct {
	kind TypeKind
	base *Type
}

func intType() *Type {
	return &Type{kind: TY_INT}
}

func pointerTo(base *Type) *Type {
	return &Type{kind: TY_PTR, base: base}
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
	case ND_MUL, ND_DIV, ND_EQ, ND_NE, ND_LT, ND_LE, ND_VAR, ND_FUNCALL, ND_NUM:
		node.ty = intType()
		return
	case ND_ADD:
		if node.rhs.ty.kind == TY_PTR {
			tmp := node.lhs
			node.lhs = node.rhs
			node.rhs = tmp
		}
		if node.rhs.ty.kind == TY_PTR {
			errorTok(node.tok, "invalid pointer arithmetic operands")
		}
		node.ty = node.lhs.ty
		return
	case ND_SUB:
		if node.rhs.ty.kind == TY_PTR {
			errorTok(node.tok, "invalid pointer arithmetic operands")
		}
		node.ty = node.lhs.ty
		return
	case ND_ASSIGN:
		node.ty = node.lhs.ty
		return
	case ND_ADDR:
		node.ty = pointerTo(node.lhs.ty)
		return
	case ND_DEREF:
		if node.lhs.ty.kind == TY_PTR {
			node.ty = node.lhs.ty.base
		} else {
			node.ty = intType()
		}
		return
	}
}

func addType(prog *Function) {
	for fn := prog; fn != nil; fn = fn.next {
		for node := fn.node; node != nil; node = node.next {
			visit(node)
		}
	}
}

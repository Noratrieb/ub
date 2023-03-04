use std::fmt::Write;

use crate::ast::{
    BinOpKind, ElsePart, Expr, ExprKind, File, IfStmt, Item, Literal, NameTyPair, Stmt, Ty, TyKind,
    UnaryOpKind,
};

pub fn pretty_print_ast(ast: &File) -> String {
    let mut printer = Printer {
        out: String::new(),
        indent: 0,
    };

    printer.print_items(&ast.items);

    printer.out
}

struct Printer {
    out: String,
    indent: usize,
}
impl Printer {
    fn print_items(&mut self, items: &[Item]) {
        for item in items {
            self.print_item(item);
        }
    }

    fn print_item(&mut self, item: &Item) {
        match item {
            Item::FnDecl(fn_decl) => {
                self.word("fn ");
                self.word(&fn_decl.name);
                self.word("(");
                let params = &fn_decl.params;
                if params.len() > 0 {
                    self.linebreak_indent();
                    for i in 0..params.len().saturating_sub(1) {
                        let param = &fn_decl.params[i];
                        self.print_name_ty(param);
                        self.word(",");
                        self.linebreak();
                    }
                    if params.len() > 0 {
                        self.print_name_ty(params.last().unwrap());
                    }
                    self.linebreak_unindent();
                }

                self.word(") ");
                if let Some(ret_ty) = &fn_decl.ret_ty {
                    self.word("-> ");
                    self.print_ty(ret_ty);
                    self.word(" ");
                }
                self.print_block(&fn_decl.body);
                self.linebreak();
            }
            Item::StructDecl(_) => {
                todo!()
            }
        }
    }

    fn print_name_ty(&mut self, name_ty: &NameTyPair) {
        self.word(&name_ty.name);
        self.word(": ");
        self.print_ty(&name_ty.ty);
    }

    fn print_ty(&mut self, ty: &Ty) {
        match &ty.kind {
            TyKind::Name(name) => self.word(name),
            TyKind::Ptr(ty) => {
                self.word("ptr ");
                self.print_ty(ty);
            }
        }
    }

    /// Starts off after the `{`. Indents itself first if necessary. Stops at the place where `}` should be inserted.
    fn print_block(&mut self, stmts: &[Stmt]) {
        self.word("{");
        if let [first, rest @ ..] = &stmts {
            self.linebreak_indent();
            self.print_stmt(first);

            for stmt in rest {
                self.linebreak();
                self.print_stmt(stmt);
            }

            self.linebreak_unindent();
        }
        self.word("}");
    }

    /// Leaves the cursor after the semicolon
    fn print_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::VarDecl(decl) => {
                self.word("let ");
                self.word(&decl.name);
                if let Some(ty) = &decl.ty {
                    self.word(": ");
                    self.print_ty(ty);
                }
                if let Some(rhs) = &decl.rhs {
                    self.word(" = ");
                    self.print_expr(rhs);
                }
                self.word(";");
            }
            Stmt::Assignment(assign) => {
                self.print_expr(&assign.place);
                self.word(" = ");
                self.print_expr(&assign.rhs);
                self.word(";");
            }
            Stmt::IfStmt(if_stmt) => {
                self.print_if(if_stmt);
            }
            Stmt::WhileStmt(while_stmt) => {
                self.word("while ");
                self.print_expr(&while_stmt.cond);
                self.print_block(&while_stmt.body);
            }
            Stmt::LoopStmt(loop_stmt) => {
                self.word("loop ");
                self.print_block(&loop_stmt.body);
            }
            Stmt::Item(item) => {
                self.print_item(item);
            }
            Stmt::Expr(expr) => {
                self.print_expr(expr);
                self.word(";");
            }
        }
    }

    fn print_if(&mut self, if_stmt: &IfStmt) {
        self.word("if ");
        self.print_expr(&if_stmt.cond);
        self.word(" ");
        self.print_block(&if_stmt.body);
        if let Some(else_part) = &if_stmt.else_part {
            self.word(" else ");
            match else_part {
                ElsePart::Else(stmts, _) => {
                    self.print_block(stmts);
                }
                ElsePart::ElseIf(if_stmt) => {
                    self.print_if(if_stmt);
                }
            }
        }
    }

    fn print_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::BinOp(bin_op) => {
                self.print_expr_wrapped(&bin_op.lhs);
                self.word(match bin_op.kind {
                    BinOpKind::Eq => "  == ",
                    BinOpKind::Neq => " != ",
                    BinOpKind::Gt => " > ",
                    BinOpKind::Lt => " < ",
                    BinOpKind::GtEq => " >= ",
                    BinOpKind::LtEq => " <= ",
                    BinOpKind::Add => " + ",
                    BinOpKind::Sub => " - ",
                    BinOpKind::Mul => " * ",
                    BinOpKind::Div => " / ",
                    BinOpKind::Mod => " % ",
                    BinOpKind::Shr => " >> ",
                    BinOpKind::Shl => " << ",
                    BinOpKind::And => " && ",
                    BinOpKind::Or => " || ",
                    BinOpKind::BitAnd => " & ",
                    BinOpKind::BitOr => " | ",
                    BinOpKind::Xor => " ^ ",
                });
                self.print_expr_wrapped(&bin_op.rhs);
            }
            ExprKind::UnaryOp(unary_op) => {
                self.word(match unary_op.kind {
                    UnaryOpKind::Not => "!",
                    UnaryOpKind::Neg => "-",
                    UnaryOpKind::Deref => "*",
                    UnaryOpKind::AddrOf => "&",
                });
                self.print_expr_wrapped(&unary_op.expr);
            }
            ExprKind::FieldAccess(field_access) => {
                self.print_expr(&field_access.expr);
                self.word(".");
                self.word(&field_access.field_name);
            }
            ExprKind::Call(call) => {
                self.print_expr(&call.callee);
                self.word("(");
                if let [first, rest @ ..] = &*call.args {
                    self.print_expr(first);
                    for expr in rest {
                        self.word(", ");
                        self.print_expr(expr);
                    }
                }
                self.word(")");
            }
            ExprKind::Literal(literal) => match literal {
                Literal::Integer(int, _) => write!(self.out, "{int}").unwrap(),
                Literal::String(string, _) => {
                    self.word("\"");
                    // FIXME: Handle escapes.
                    self.word(string);
                    self.word("\"");
                }
            },
            ExprKind::Name(name) => {
                self.word(name);
            }
            ExprKind::Array(exprs) => {
                self.word("[");
                if let [first, rest @ ..] = exprs.as_slice() {
                    self.print_expr(first);
                    for expr in rest {
                        self.word(", ");
                        self.print_expr(expr);
                    }
                }
                self.word("]");
            }
        }
    }

    fn print_expr_wrapped(&mut self, expr: &Expr) {
        match expr.kind {
            ExprKind::Literal(_)
            | ExprKind::Array(_)
            | ExprKind::Call(_)
            | ExprKind::Name(_)
            | ExprKind::FieldAccess(_) => {
                self.print_expr(expr);
            }
            ExprKind::BinOp(_) | ExprKind::UnaryOp(_) => {
                self.word("(");
                self.print_expr(expr);
                self.word(")");
            }
        }
    }

    // utility functions

    fn word(&mut self, word: &str) {
        self.out.push_str(word);
    }

    fn linebreak_indent(&mut self) {
        self.indent += 1;
        self.linebreak();
    }

    fn linebreak_unindent(&mut self) {
        self.indent -= 1;
        self.linebreak();
    }

    fn linebreak(&mut self) {
        self.word("\n");
        self.word(&"    ".repeat(self.indent))
    }
}

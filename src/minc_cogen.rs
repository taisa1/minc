use crate::minc_ast;
use std::{collections::HashMap, hash::Hash};

struct Assembly {
    body: String,
}

impl Assembly {
    fn push_asm(&mut self, asm: Assembly) {
        self.body.push_str(asm.body.as_str());
    }
    fn push_line(&mut self, line: &str) {
        self.body.push_str(line);
        self.body.push('\n');
    }
}

#[allow(unreachable_code, unused_variables)]
pub fn ast_to_asm_program(_program: minc_ast::Program) -> String {
    let mut asm: String = String::new();
    let mut Lnum = 0;
    for def in _program.defs {
        asm.push_str(&ast_to_asm_def(def, &mut Lnum));
    }
    asm.to_string()
}

pub fn ast_to_asm_def(def: minc_ast::Def, Lnum: &mut usize) -> String {
    let mut asm = String::new();
    match def {
        minc_ast::Def::Fun(ref name, ref params, ref ret_type, ref body) => {
            let mut env: HashMap<String, String> = HashMap::new();
            let mut v: usize = 8;
            asm.push_str(&gen_prologue(&def, &mut env, &mut v).body);
            asm.push_str(&ast_to_asm_stmt(body, &mut env, v, Lnum).body);
            asm.push_str(&gen_epilogue(&def).body);
        }
    }
    asm
}

pub fn gen_prologue(
    def: &minc_ast::Def,
    env: &mut HashMap<String, String>,
    v: &mut usize,
) -> Assembly {
    //TODO: grow the stack
    let mut res: Assembly = Assembly {
        body: String::new(),
    };
    match def {
        minc_ast::Def::Fun(ref name, ref params, ref ret_type, ref body) => {
            res.push_line(format!(".file\t\"{}.c\"", name).as_str());
            res.push_line(format!(".text").as_str());
            res.push_line(format!(".p2align 4,,15").as_str());
            res.push_line(format!(".globl {}", name).as_str());
            res.push_line(format!(".type {}, @function", name).as_str());
            res.push_line(format!("{}:", name).as_str());
            res.push_line(format!("LFB0:").as_str());
            res.push_line(format!(".cfi_startproc").as_str());
            let mut i: usize = 0;
            for param in params {
                let loc = match i {
                    0 => format!("%rdi"),
                    1 => format!("%rsi"),
                    2 => format!("%rdx"),
                    3 => format!("%rcx"),
                    4 => format!("%r8"),
                    5 => format!("%r9"),
                    _ => format!("{}(%rsp)", (i - 5) * 8),
                };
                env_add(param.name.clone(), loc, env);
                if i > 5 {
                    *v = (i - 4) * 8;
                }
                i += 1;
            }
        }
    }
    res
}

pub fn gen_epilogue(def: &minc_ast::Def) -> Assembly {
    //TODO: shrink the stack, ret
    let mut res: Assembly = Assembly {
        body: String::new(),
    };
    match def {
        minc_ast::Def::Fun(ref name, ref params, ref ret_type, ref body) => {
            res.push_line(format!(".cfi_endproc").as_str());
            res.push_line(format!(".LFE0:").as_str());
            res.push_line(format!(".size {}, .-{}", name, name).as_str());
            res.push_line(
                format!(".ident \"GCC: (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0\"").as_str(),
            );
            res.push_line(format!(".section	.note.GNU-stack,\"\",@progbits").as_str());
        }
    }
    res
}

pub fn ast_to_asm_stmt(
    stmt: &minc_ast::Stmt,
    env: &mut HashMap<String, String>,
    v: usize,
    Lnum: &mut usize,
) -> Assembly {
    let mut res: Assembly = Assembly {
        body: String::new(),
    };
    match stmt {
        minc_ast::Stmt::Empty => {}
        minc_ast::Stmt::Continue => {
            //TODO
            /*
            jmp Lc
            */
        }
        minc_ast::Stmt::Break => {
            //TODO
            /*
            jmp Lb
             */
        }
        minc_ast::Stmt::Return(expr) => {
            let (ret_op, ret_insns) = ast_to_asm_expr(&expr, env, v);
            /*
            <ret_insns>
            movq ret_op %rax
            ret
             */
            res.push_asm(ret_insns);
            res.push_line(format!("movq {}, %rax", ret_op).as_str());
            res.push_line("ret");
        }
        minc_ast::Stmt::Expr(expr) => {
            let (_, expr_insns) = ast_to_asm_expr(&expr, env, v);
            /*
            <expr_insns>
             */
            res.push_asm(expr_insns);
        }
        minc_ast::Stmt::Compound(decls, stmts) => {
            let (mut new_env, new_v) = env_extend(decls, &v, env);
            println!("{}", new_v);
            for stmt in stmts {
                res.push_asm(ast_to_asm_stmt(stmt, &mut new_env, new_v, Lnum));
            }
        }
        minc_ast::Stmt::If(cond, then_stmt, Some(else_stmt)) => {
            let (cond_op, cond_insns) = ast_to_asm_expr(&cond, env, v);
            let then_insns = ast_to_asm_stmt(then_stmt, env, v, Lnum);
            let else_insns = ast_to_asm_stmt(else_stmt, env, v, Lnum);
            /*
                <cond_insns>
                cmpq $0, cond_op
                je L
                <then_insns>
                j Le
            L:
                <else_stmt>
            Le:
             */
            *Lnum += 1;
            res.push_asm(cond_insns);
            res.push_line(format!("cmpq $0, {}", cond_op).as_str());
            res.push_line(format!("je L{}", Lnum).as_str()); //TODO:L_X
            res.push_asm(then_insns);
            res.push_line(format!("jmp L{}", *Lnum + 1).as_str());
            res.push_line(format!("L{}:", Lnum).as_str());
            res.push_asm(else_insns);
            res.push_line(format!("L{}:", *Lnum + 1).as_str());
            *Lnum += 1;
        }
        minc_ast::Stmt::If(cond, then_stmt, None) => {
            let (cond_op, cond_insns) = ast_to_asm_expr(&cond, env, v);
            let then_insns = ast_to_asm_stmt(then_stmt, env, v, Lnum);
            /*
                <cond_insns>
                cmpq $0, cond_op
                je L
                <then_insns>
            L:
             */
            *Lnum += 1;
            res.push_asm(cond_insns);
            res.push_line(format!("cmpq $0, {}", cond_op).as_str());
            res.push_line(format!("je L{}", Lnum).as_str()); //TODO:L_X
            res.push_asm(then_insns);
            res.push_line(format!("L{}:", Lnum).as_str());
        }
        minc_ast::Stmt::While(cond, body) => {
            let (cond_op, cond_insns) = ast_to_asm_expr(&cond, env, v);
            let body_insns = ast_to_asm_stmt(body, env, v, Lnum);
            /*
            jmp Lc
            Ls:
                <body_insns>
            Lc:
                <cond_insns>
                cmpq $0, cond_op
                jne Ls
            Lb:
                nop
            */
            *Lnum += 1;
            res.push_line(format!("jmp Lc{}", Lnum).as_str());
            res.push_line(format!("Ls{}:", Lnum).as_str());
            res.push_asm(body_insns);
            res.push_line(format!("Lc{}:", Lnum).as_str());
            res.push_asm(cond_insns);
            res.push_line(format!("cmpq $0, {}", cond_op).as_str());
            res.push_line(format!("jne Ls{}", Lnum).as_str());
        }
    }
    res
}

//returns (op,insns)
pub fn ast_to_asm_expr(
    expr: &minc_ast::Expr,
    env: &mut HashMap<String, String>,
    v: usize,
) -> (String, Assembly) {
    let mut res_op = String::new();
    let mut res_insns = Assembly {
        body: String::new(),
    };
    match expr {
        minc_ast::Expr::IntLiteral(val) => {
            res_insns.push_line(format!("movq ${}, %rax", val).as_str());
            res_op = format!("%rax");
        }
        minc_ast::Expr::Id(name) => {
            res_op = format!("{}", &env_lookup(name, env));
        }
        minc_ast::Expr::Op(op, args) => {
            if args.len() == 1 {
                let (op0, insns0) = ast_to_asm_expr(&args[0], env, v);
                let m0 = v.to_string() + "(%rsp)";
                res_insns.push_asm(insns0);
                res_insns.push_line(format!("movq {}, {}", op0, m0).as_str());
                match op.as_str() {
                    "+" => {
                        res_op = op0;
                    }
                    "-" => {
                        res_insns.push_line(format!("movq $0, {}", op0).as_str());
                        res_insns.push_line(format!("subq {}, {}", m0, op0).as_str());
                        res_op = op0;
                    }
                    "!" => {
                        res_insns.push_line(format!("testq {}, {}", op0, op0).as_str());
                        res_insns.push_line(format!("sete %al").as_str());
                        res_insns.push_line(format!("movzbq %al, {}", op0).as_str());
                        res_op = op0;
                    }
                    _ => {}
                }
            } else if args.len() == 2 {
                let (op1, insns1) = ast_to_asm_expr(&args[1], env, v);
                let (op0, insns0) = ast_to_asm_expr(&args[0], env, v + 8);
                let m1 = v.to_string() + "(%rsp)";
                let m0 = (v + 8).to_string() + "(%rsp)";
                res_insns.push_asm(insns1);
                if &op1[..1] != "%" {
                    //avoid memory load
                    res_insns.push_line(format!("movq {}, %rax", op1).as_str());
                    res_insns.push_line(format!("movq %rax, {}", m1).as_str());
                } else {
                    res_insns.push_line(format!("movq {}, {}", op1, m1).as_str());
                }
                /*
                insns1
                movq op1, m    m <- op1
                insns0
                addq m, op0    op0 = op0 + m
                これにより>=rsp+v+8に入る
                */
                match op.as_str() {
                    "+" | "-" | "*" | "/" | "%" | "=" => {
                        res_insns.push_asm(insns0);
                        match op.as_str() {
                            "+" => {
                                if &op0[..1] == "%" {
                                    res_insns.push_line(format!("addq {}, {}", m1, op0).as_str());
                                } else {
                                    res_insns.push_line(format!("movq {}, %rax", op0).as_str());
                                    res_insns.push_line(format!("addq {}, %rax", m1).as_str());
                                    res_insns.push_line(format!("movq %rax, {}", op0).as_str());
                                }
                            }
                            "-" => {
                                if &op0[..1] == "%" {
                                    res_insns.push_line(format!("subq {}, {}", m1, op0).as_str());
                                } else {
                                    res_insns.push_line(format!("movq {}, %rax", op0).as_str());
                                    res_insns.push_line(format!("subq {}, %rax", m1).as_str());
                                    res_insns.push_line(format!("movq %rax, {}", op0).as_str());
                                }
                            }
                            "*" => {
                                if &op0[..1] == "%" {
                                    res_insns.push_line(format!("imulq {}, {}", m1, op0).as_str());
                                } else {
                                    res_insns.push_line(format!("movq {}, %rax", op0).as_str());
                                    res_insns.push_line(format!("imulq {}, %rax", m1).as_str());
                                    res_insns.push_line(format!("movq %rax, {}", op0).as_str());
                                }
                            }
                            "/" => {
                                res_insns.push_line(format!("movq {}, %rax", op0).as_str());
                                res_insns.push_line(format!("cqto").as_str());
                                res_insns.push_line(format!("idivq {}", m1).as_str());
                                res_insns.push_line(format!("movq %rax, {}", op0).as_str());
                            }
                            "%" => {
                                res_insns.push_line(format!("movq {}, %rax", op0).as_str());
                                res_insns.push_line(format!("cqto").as_str());
                                res_insns.push_line(format!("idivq {}", m1).as_str());
                                res_insns.push_line(format!("movq %rdx, {}", op0).as_str());
                            }
                            "=" => {
                                if &op0[..1] == "%" {
                                    res_insns.push_line(format!("movq {}, {}", m1, op0).as_str());
                                } else {
                                    res_insns.push_line(format!("movq {}, %rax", m1).as_str());
                                    res_insns.push_line(format!("movq %rax, {}", op0).as_str());
                                }
                            }
                            _ => {}
                        }
                        res_op = op0;
                    }
                    "<" | ">" | "==" | "<=" | ">=" | "!=" => {
                        /*
                        ex. ">"
                        insns1
                        movq op1,m1     m1 <- op1
                        insns0
                        movq op0,m0     m0 <- op0
                        movq $0,%rax    rax <- 0
                        movq m0,op0     op0 <- m0
                        cmpq m1,op0     set flag
                        setl rax        rax <- m1<op0
                         */
                        res_insns.push_asm(insns0);
                        res_insns.push_line(format!("movq {}, {}", op0, m0).as_str());
                        res_insns.push_line(format!("movq $0, %rax").as_str());
                        res_insns.push_line(format!("movq {}, {}", m0, op0).as_str());
                        res_insns.push_line(format!("cmpq {}, {}", m1, op0).as_str());
                        match op.as_str() {
                            "<" => {
                                res_insns.push_line(format!("setl %al").as_str());
                            }
                            ">" => {
                                res_insns.push_line(format!("setle %al").as_str());
                                res_insns.push_line(format!("xor $1, %al").as_str());
                            }
                            "==" => {
                                res_insns.push_line(format!("sete %al").as_str());
                            }
                            "<=" => {
                                res_insns.push_line(format!("setle %al").as_str());
                            }
                            ">=" => {
                                res_insns.push_line(format!("setl %al").as_str());
                                res_insns.push_line(format!("xor $1, %al").as_str());
                            }
                            "!=" => {
                                res_insns.push_line(format!("setne %al").as_str());
                            }
                            _ => {}
                        }
                        res_op = format!("%rax");
                    }
                    _ => {}
                }
            }
        }
        minc_ast::Expr::Call(fun, args) => {
            //
            let (fun_op, _) = ast_to_asm_expr(fun, env, v);
            res_insns.push_line(format!("call {}", fun_op).as_str());
        }
        minc_ast::Expr::Paren(sub_expr) => {
            // return as it is
            (res_op, res_insns) = ast_to_asm_expr(sub_expr, env, v);
        }
    }
    (res_op, res_insns)
}

pub fn env_lookup(name: &String, env: &mut HashMap<String, String>) -> String {
    if !env.contains_key(name) {
        panic!("key not found: {}", name);
    }
    env[name].clone()
}

pub fn env_add(name: String, loc: String, env: &mut HashMap<String, String>) {
    env.insert(name, loc);
}

pub fn env_extend(
    decls: &Vec<minc_ast::Decl>,
    loc: &usize,
    env: &HashMap<String, String>,
) -> (HashMap<String, String>, usize) {
    let mut new_env = env.clone();
    let mut new_v: usize = loc.clone();
    for decl in decls {
        env_add(decl.name.clone(), format!("{}(%rsp)", new_v), &mut new_env);
        new_v += 8;
    }
    (new_env, new_v)
}

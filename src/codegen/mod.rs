//! LLVM code generation
// Copyright (c) Alexander Koch 2018
#[macro_use]
pub mod macros;
pub mod llvm;

use llvm_sys::prelude::LLVMValueRef;
use llvm_sys::LLVMIntPredicate;
use parser::ast;
use std::collections::HashMap;
use lexer::{Error, Position};
pub type CodegenResult<T> = ::std::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub struct ProgramConfig<'a> {
    pub prelude: Option<ast::Program>,
    pub output: &'a str
}

#[derive(Debug, Clone)]
pub struct Codegen {
    ctx: llvm::Context,
    builder: llvm::Builder,
    named_values: HashMap<String, LLVMValueRef>,
}

fn is_constant(value: &str) -> bool {
    value.chars().next().map(|c| c.is_digit(10)).unwrap_or(false)
}

impl Codegen {
    pub fn new() -> Codegen {
        let ctx = llvm::Context::new();
        let builder = ctx.create_builder();
        Codegen {
            ctx: ctx,
            builder: builder,
            named_values: HashMap::new()
        }
    }

    pub fn collect_variables(&mut self, program: &ast::Program, set: &mut HashMap<String, bool>) {
        match program.kind {
            ast::ProgramKind::Assignment(ref assignee, ref lhs, _, ref rhs) => {
                if !is_constant(lhs) && !set.contains_key(lhs) {
                    set.insert(lhs.to_owned(), true);
                }
                if !is_constant(rhs) && !set.contains_key(rhs) {
                    set.insert(rhs.to_owned(), true);
                }
                if !set.contains_key(assignee) {
                    set.insert(assignee.to_owned(), false);
                }
            },
            ast::ProgramKind::Loop(ref ident, ref p) => {
                if !set.contains_key(ident) {
                    set.insert(ident.to_owned(), true);
                }
                self.collect_variables(p, set)
            },
            ast::ProgramKind::Chain(ref p1, ref p2) => {
                self.collect_variables(p1, set);
                self.collect_variables(p2, set)
            }
        }
    }

    pub fn allocate_variables(&mut self, program: &ast::Program) {
        let mut variables = HashMap::new();
        self.collect_variables(program, &mut variables);
        debug!("Vars: {:?}", variables);
        for (variable, clear) in variables {
            let mem = self.builder.build_alloca(self.ctx.int64_ty());
            if clear {
                let zero = llvm::const_int(self.ctx.int64_ty(), 0, false);
                self.builder.build_store(zero, mem);
            }
            self.named_values.insert(variable, mem);
        }
    }

    pub fn compile(&mut self, name: &str, config: &ProgramConfig, program: ast::Program) -> CodegenResult<llvm::Module> {
        let mut module = llvm::Module::new(&self.ctx, &name);
        let func = module.add_function("main", &mut vec![], self.ctx.int64_ty(), false);
        let block = self.ctx.append_basic_block(func, "entry");
        self.builder.position_at_end(block);

        // Merge the two programs if needed
        let p = if let Some(ref p) = config.prelude {
            ast::Program {
                position: Position::new(0, 0),
                kind: ast::ProgramKind::Chain(Box::new(p.clone()), Box::new(program))
            }
        } else {
            program
        };

        // Compile the main program and set the output variable
        self.allocate_variables(&p);
        try!(self.codegen_program(&mut module, &p));
        let ret = if let Some(x) = self.named_values.get(config.output) {
            self.builder.build_load(*x)
        } else {
            llvm::const_int(self.ctx.int64_ty(), 0, false)
        };

        self.builder.build_ret(Some(ret));
        Ok(module)
    }

    fn codegen_program(&mut self, module: &mut llvm::Module, program: &ast::Program) -> CodegenResult<()> {
        debug!("Codegen: program");
        let position = &program.position;
        match program.kind {
            ast::ProgramKind::Assignment(ref assignee, ref lhs, ref op, ref rhs) => {
                self.codegen_assignment(position, assignee, lhs, op, rhs)
            },
            ast::ProgramKind::Loop(ref ident, ref program) => {
                self.codegen_loop(module, position, ident, program)
            },
            ast::ProgramKind::Chain(ref p1, ref p2) => {
                try!(self.codegen_program(module, &p1));
                self.codegen_program(module, &p2)
            }
        }
    }

    fn get_variable_ref(&mut self, position: &Position, ident: &str) -> CodegenResult<LLVMValueRef> {
        match self.named_values.get(ident) {
            Some(v) => Ok(*v),
            None => Err(Error {
                position: *position,
                message: format!("Variable '{}' is not defined", ident)
            })
        }
    }

    fn codegen_value(&mut self, position: &Position, value: &str) -> CodegenResult<LLVMValueRef> {
        if is_constant(value) {
            let v = value.parse::<i64>().unwrap();
            Ok(llvm::const_int(self.ctx.int64_ty(), v as usize, false))
        } else {
            let val = try!(self.get_variable_ref(position, value));
            Ok(self.builder.build_load(val))
        }
    }

    fn codegen_assignment(&mut self,
                    position: &Position,
                    assignee: &str,
                    lhs: &str,
                    op: &ast::BinaryOperator,
                    rhs: &str) -> CodegenResult<()> {
        debug!("Codegen: assignment");
        let addr = try!(self.get_variable_ref(position, assignee));
        let lhs = try!(self.codegen_value(position, lhs));
        let expr = match *op {
            ast::BinaryOperator::Nop => lhs,
            ast::BinaryOperator::Plus => {
                let rhs = try!(self.codegen_value(position, rhs));
                self.builder.build_add(lhs, rhs, false)
            },
            ast::BinaryOperator::Minus => {
                let rhs = try!(self.codegen_value(position, rhs));
                let res = self.builder.build_sub(lhs, rhs, false);
                let cmp = self.builder.build_icmp(LLVMIntPredicate::LLVMIntULE, res, lhs);
                let zext = self.builder.build_zext(cmp, self.ctx.int64_ty());
                let neg = self.builder.build_sub_nsw(llvm::const_int(self.ctx.int64_ty(), 0, false), zext);
                self.builder.build_and(res, neg)
            }
        };
        self.builder.build_store(expr, addr);
        Ok(())
    }

    fn codegen_loop(&mut self,
                    module: &mut llvm::Module,
                    position: &Position,
                    condition: &str,
                    program: &ast::Program) -> CodegenResult<()> {
        debug!("Codegen: loop");
        let counter = self.builder.build_alloca(self.ctx.int64_ty());
        let cond = try!(self.codegen_value(position, condition));
        self.builder.build_store(cond, counter);

        let zero = llvm::const_int(self.ctx.int64_ty(), 0, false);
        let sub = llvm::const_int(self.ctx.int64_ty(), usize::max_value(), false);
        let cnt_idx0 = self.builder.build_load(counter);
        let cmp0 = self.builder.build_icmp(LLVMIntPredicate::LLVMIntUGT, cnt_idx0, zero);

        let func = match module.get_function("main") {
            Some(f) => f,
            None => return Err(Error {
                position: *position,
                message: "Main function is missing".into()
            })
        };

        let then_block = self.ctx.append_basic_block(func, "L");
        let else_block = self.ctx.append_basic_block(func, "L");
        self.builder.build_cond_br(cmp0, then_block, else_block);

        self.builder.position_at_end(then_block);
        try!(self.codegen_program(module, program));
        let cnt_idx1 = self.builder.build_load(counter);
        let cnt_update = self.builder.build_add_nsw(cnt_idx1, sub);
        self.builder.build_store(cnt_update, counter);

        let cmp1 = self.builder.build_icmp(LLVMIntPredicate::LLVMIntUGT, cnt_update, zero);
        self.builder.build_cond_br(cmp1, then_block, else_block);

        self.builder.position_at_end(else_block);
        Ok(())
    }
}

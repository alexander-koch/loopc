//! LLVM code generation
// Copyright (c) Alexander Koch 2018
#[macro_use]
pub mod macros;
pub mod llvm;

use llvm_sys::prelude::LLVMValueRef;
use llvm_sys::LLVMIntPredicate;
use parser::ast;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
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

    pub fn compile(&mut self, name: &str, config: &ProgramConfig, program: &ast::Program) -> CodegenResult<llvm::Module> {
        let mut module = llvm::Module::new(&self.ctx, &name);
        let func = module.add_function("main", &mut vec![], self.ctx.int64_ty(), false);
        let block = self.ctx.append_basic_block(func, "entry");
        self.builder.position_at_end(block);

        // Compile the 'prelude'
        if let Some(ref p) = config.prelude {
            try!(self.codegen_program(&mut module, p));
        }

        // Compile the main program and set the output variable
        try!(self.codegen_program(&mut module, program));
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

    fn codegen_value(&mut self, position: &Position, value: &str) -> CodegenResult<LLVMValueRef> {
         match value.chars().next() {
             Some(c) => {
                 if c.is_digit(10) {
                     let v = value.parse::<i64>().unwrap();
                     Ok(llvm::const_int(self.ctx.int64_ty(), v as usize, false))
                 } else {
                     match self.named_values.get(value) {
                         Some(v) => Ok(self.builder.build_load(*v)),
                         None => Err(Error {
                             position: *position,
                             message: format!("Value '{}' has not been initialized", value)
                         })
                     }
                 }
             },
             None => panic!("Assignment value is invalid")
         }
    }

    fn codegen_assignment(&mut self,
                    position: &Position,
                    assignee: &str,
                    lhs: &str,
                    op: &ast::BinaryOperator,
                    rhs: &str) -> CodegenResult<()> {
        debug!("Codegen: assignment");
        let alloc = *match self.named_values.entry(assignee.to_owned()) {
            Entry::Occupied(o) => o.into_mut(),
            Entry::Vacant(v) => {
                let mem = self.builder.build_alloca(self.ctx.int64_ty());
                v.insert(mem)
            }
        };

        let lhs = try!(self.codegen_value(position, lhs));
        let expr = match *op {
            ast::BinaryOperator::Nop => lhs,
            ast::BinaryOperator::Plus => {
                let rhs = try!(self.codegen_value(position, rhs));
                self.builder.build_add(lhs, rhs, false)
            },
            ast::BinaryOperator::Minus => {
                let rhs = try!(self.codegen_value(position, rhs));
                self.builder.build_sub(lhs, rhs, false)
            }
        };
        self.builder.build_store(expr, alloc);
        Ok(())
    }

    fn codegen_loop(&mut self,
                    module: &mut llvm::Module,
                    position: &Position,
                    condition: &str,
                    program: &ast::Program) -> CodegenResult<()> {
        debug!("Codegen: loop");
        let counter = self.builder.build_alloca(self.ctx.int64_ty());
        match self.named_values.get(condition) {
            Some(v) => {
                let val = self.builder.build_load(*v);
                self.builder.build_store(val, counter);
            },
            None => return Err(Error {
                position: *position,
                message: format!("Conditional variable '{}' is uninitialized", condition)
            })
        }
        let zero = llvm::const_int(self.ctx.int64_ty(), 0, false);
        let sub = llvm::const_int(self.ctx.int64_ty(), usize::max_value(), false);
        let cnt_idx0 = self.builder.build_load(counter);
        let cmp0 = self.builder.build_icmp(LLVMIntPredicate::LLVMIntSGT, cnt_idx0, zero);

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

        let cmp1 = self.builder.build_icmp(LLVMIntPredicate::LLVMIntSGT, cnt_update, zero);
        self.builder.build_cond_br(cmp1, then_block, else_block);

        self.builder.position_at_end(else_block);
        Ok(())
    }
}

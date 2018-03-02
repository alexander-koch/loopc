//! Rust LLVM bindings
// Copyright (C) 2018 Alexander Koch
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::analysis::*;
use llvm_sys::target::*;
use llvm_sys::target_machine::*;
use llvm_sys::execution_engine::*;
use llvm_sys::{LLVMIntPredicate, LLVMRealPredicate, LLVMTypeKind, LLVMValueKind};
use libc::{c_char, c_double, c_uint, c_ulonglong};

use std::ffi::{CStr, CString};
use std::fmt;
use std::mem;

static EMPTY_STR: [c_char; 1] = [0];
const LLVM_FALSE: LLVMBool = 0;
const LLVM_TRUE: LLVMBool = 1;
macro_rules! llvm_bool { ($name:expr) => ( if $name { LLVM_TRUE } else { LLVM_FALSE } ) }

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Context(LLVMContextRef);
native_eq!(Context, LLVMContextRef);
finalizer!(Context, LLVMContextDispose);

impl Context {
    pub fn new() -> Context {
        unsafe { LLVMContextCreate().into() }
    }

    pub fn create_builder(&self) -> Builder {
        unsafe { LLVMCreateBuilderInContext(self.into()).into() }
    }

    pub fn append_basic_block(&mut self, func: LLVMValueRef, name: &str) -> LLVMBasicBlockRef {
        let c_name = CString::new(name).unwrap();
        let block_name = c_name.to_bytes_with_nul().as_ptr() as *const c_char;
        unsafe { LLVMAppendBasicBlockInContext(self.into(), func, block_name) }
    }

    pub fn void_ty(&self) -> LLVMTypeRef {
        unsafe { LLVMVoidTypeInContext(self.into()) }
    }

    pub fn float_ty(&self) -> LLVMTypeRef {
        unsafe { LLVMFloatTypeInContext(self.into()) }
    }

    pub fn double_ty(&self) -> LLVMTypeRef {
        unsafe { LLVMDoubleTypeInContext(self.into()) }
    }

    pub fn int1_ty(&self) -> LLVMTypeRef {
        unsafe { LLVMInt1TypeInContext(self.into()) }
    }

    pub fn int8_ty(&self) -> LLVMTypeRef {
        unsafe { LLVMInt8TypeInContext(self.into()) }
    }

    pub fn int32_ty(&self) -> LLVMTypeRef {
        unsafe { LLVMInt32TypeInContext(self.into()) }
    }

    pub fn int64_ty(&self) -> LLVMTypeRef {
        unsafe { LLVMInt64TypeInContext(self.into()) }
    }

    pub fn struct_ty(&self, types: &mut Vec<LLVMTypeRef>, packed: bool) -> LLVMTypeRef {
        unsafe {
            LLVMStructTypeInContext(
                self.into(),
                types.as_mut_ptr(),
                types.len() as c_uint,
                llvm_bool!(packed),
            )
        }
    }

    pub fn void_ptr_ty(&self) -> LLVMTypeRef {
        pointer_ty(self.int8_ty(), 0)
    }

    pub fn const_float(&self, val: f64) -> LLVMValueRef {
        const_real(self.float_ty(), val)
    }

    pub fn const_double(&self, val: f64) -> LLVMValueRef {
        const_real(self.double_ty(), val)
    }
}

// types

pub fn pointer_ty(base: LLVMTypeRef, address: usize) -> LLVMTypeRef {
    unsafe { LLVMPointerType(base, address as c_uint) }
}

pub fn function_ty(args: &mut Vec<LLVMTypeRef>, ret: LLVMTypeRef, variadic: bool) -> LLVMTypeRef {
    unsafe {
        LLVMFunctionType(
            ret,
            args.as_mut_ptr(),
            args.len() as c_uint,
            llvm_bool!(variadic),
        )
    }
}

// values

pub fn const_null(ty: LLVMTypeRef) -> LLVMValueRef {
    unsafe { LLVMConstNull(ty) }
}

pub fn const_int(ty: LLVMTypeRef, val: usize, sign_extend: bool) -> LLVMValueRef {
    unsafe { LLVMConstInt(ty, val as c_ulonglong, llvm_bool!(sign_extend)) }
}

pub fn const_real(ty: LLVMTypeRef, val: f64) -> LLVMValueRef {
    unsafe { LLVMConstReal(ty, val as c_double) }
}

// helper

pub fn type_of(val: LLVMValueRef) -> LLVMTypeRef {
    unsafe { LLVMTypeOf(val) }
}

pub fn get_type_kind(ty: LLVMTypeRef) -> LLVMTypeKind {
    unsafe { LLVMGetTypeKind(ty) }
}

pub fn get_value_kind(val: LLVMValueRef) -> LLVMValueKind {
    unsafe { LLVMGetValueKind(val) }
}

pub fn set_value_name(val: LLVMValueRef, name: &str) {
    let c_name = CString::new(name).unwrap();
    let val_name = c_name.to_bytes_with_nul().as_ptr() as *const c_char;
    unsafe { LLVMSetValueName(val, val_name) }
}

pub fn get_param(func: LLVMValueRef, index: usize) -> LLVMValueRef {
    unsafe { LLVMGetParam(func, index as c_uint) }
}

pub fn get_params(func: LLVMValueRef) -> Vec<LLVMValueRef> {
    let param_count = unsafe { LLVMCountParams(func) };
    let mut params = Vec::new();
    for i in 0..param_count {
        params.push(unsafe { LLVMGetParam(func, i as c_uint) })
    }
    params
}

pub fn add_incoming(phi: LLVMValueRef, incoming: &mut Vec<(LLVMValueRef, LLVMBasicBlockRef)>) {
    unsafe {
        let mut values = incoming.iter().map(|x| x.0).collect::<Vec<LLVMValueRef>>();
        let mut blocks = incoming
            .iter()
            .map(|x| x.1)
            .collect::<Vec<LLVMBasicBlockRef>>();
        LLVMAddIncoming(
            phi,
            values.as_mut_ptr(),
            blocks.as_mut_ptr(),
            incoming.len() as c_uint,
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Builder(LLVMBuilderRef);
native_eq!(Builder, LLVMBuilderRef);
finalizer!(Builder, LLVMDisposeBuilder);

impl Builder {
    pub fn position_at_end(&mut self, block: LLVMBasicBlockRef) {
        unsafe { LLVMPositionBuilderAtEnd(self.into(), block) }
    }

    pub fn get_insert_block(&self) -> LLVMBasicBlockRef {
        unsafe { LLVMGetInsertBlock(self.into()).into() }
    }

    pub fn build_alloca(&mut self, ty: LLVMTypeRef) -> LLVMValueRef {
        unsafe { LLVMBuildAlloca(self.into(), ty, EMPTY_STR.as_ptr() as *const c_char) }
    }

    pub fn build_load(&mut self, val: LLVMValueRef) -> LLVMValueRef {
        unsafe { LLVMBuildLoad(self.into(), val, EMPTY_STR.as_ptr() as *const c_char) }
    }

    pub fn build_store(&mut self, val: LLVMValueRef, ptr: LLVMValueRef) -> LLVMValueRef {
        unsafe { LLVMBuildStore(self.into(), val, ptr) }
    }

    unary_op!(build_not, LLVMBuildNot);
    unary_op!(build_neg, LLVMBuildNeg, LLVMBuildFNeg);

    bin_op!(build_add, LLVMBuildAdd, LLVMBuildFAdd);
    bin_op!(build_sub, LLVMBuildSub, LLVMBuildFSub);
    bin_op!(build_mul, LLVMBuildMul, LLVMBuildFMul);
    bin_op!(build_div, LLVMBuildSDiv, LLVMBuildFDiv);
    bin_op!(build_rem, LLVMBuildURem, LLVMBuildFRem);

    pub fn build_and(&mut self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        unsafe { LLVMBuildAnd(self.into(), lhs, rhs, EMPTY_STR.as_ptr() as *const c_char) }
    }

    pub fn build_zext(&mut self, val: LLVMValueRef, dest_ty: LLVMTypeRef) -> LLVMValueRef {
        unsafe {
            LLVMBuildZExt(
                self.into(),
                val,
                dest_ty,
                EMPTY_STR.as_ptr() as *const c_char,
            )
        }
    }

    pub fn build_add_nsw(&mut self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        unsafe { LLVMBuildNSWAdd(self.into(), lhs, rhs, EMPTY_STR.as_ptr() as *const c_char) }
    }

    pub fn build_sub_nsw(&mut self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        unsafe { LLVMBuildNSWSub(self.into(), lhs, rhs, EMPTY_STR.as_ptr() as *const c_char) }
    }

    pub fn build_icmp(
        &mut self,
        op: LLVMIntPredicate,
        lhs: LLVMValueRef,
        rhs: LLVMValueRef,
    ) -> LLVMValueRef {
        unsafe {
            LLVMBuildICmp(
                self.into(),
                op,
                lhs,
                rhs,
                EMPTY_STR.as_ptr() as *const c_char,
            )
        }
    }

    pub fn build_fcmp(
        &mut self,
        op: LLVMRealPredicate,
        lhs: LLVMValueRef,
        rhs: LLVMValueRef,
    ) -> LLVMValueRef {
        unsafe {
            LLVMBuildFCmp(
                self.into(),
                op,
                lhs,
                rhs,
                EMPTY_STR.as_ptr() as *const c_char,
            )
        }
    }

    pub fn build_br(&mut self, block: LLVMBasicBlockRef) -> LLVMValueRef {
        unsafe { LLVMBuildBr(self.into(), block) }
    }

    pub fn build_cond_br(
        &mut self,
        cond: LLVMValueRef,
        if_block: LLVMBasicBlockRef,
        else_block: LLVMBasicBlockRef,
    ) -> LLVMValueRef {
        unsafe { LLVMBuildCondBr(self.into(), cond, if_block, else_block) }
    }

    pub fn build_gep(&mut self, ptr: LLVMValueRef, args: &mut Vec<LLVMValueRef>) -> LLVMValueRef {
        unsafe {
            LLVMBuildGEP(
                self.into(),
                ptr,
                args.as_mut_ptr(),
                args.len() as c_uint,
                EMPTY_STR.as_ptr() as *const c_char,
            )
        }
    }

    pub fn build_gep_in_bounds(
        &mut self,
        ptr: LLVMValueRef,
        args: &mut Vec<LLVMValueRef>,
    ) -> LLVMValueRef {
        unsafe {
            LLVMBuildInBoundsGEP(
                self.into(),
                ptr,
                args.as_mut_ptr(),
                args.len() as c_uint,
                EMPTY_STR.as_ptr() as *const c_char,
            )
        }
    }

    pub fn build_bit_cast(&mut self, val: LLVMValueRef, dest_ty: LLVMTypeRef) -> LLVMValueRef {
        unsafe {
            LLVMBuildBitCast(
                self.into(),
                val,
                dest_ty,
                EMPTY_STR.as_ptr() as *const c_char,
            )
        }
    }

    pub fn build_ptr_to_int(&mut self, val: LLVMValueRef, dest_ty: LLVMTypeRef) -> LLVMValueRef {
        unsafe {
            LLVMBuildPtrToInt(
                self.into(),
                val,
                dest_ty,
                EMPTY_STR.as_ptr() as *const c_char,
            )
        }
    }

    pub fn build_malloc(&mut self, ty: LLVMTypeRef) -> LLVMValueRef {
        unsafe { LLVMBuildMalloc(self.into(), ty, EMPTY_STR.as_ptr() as *const c_char) }
    }

    pub fn build_free(&mut self, val: LLVMValueRef) -> LLVMValueRef {
        unsafe { LLVMBuildFree(self.into(), val) }
    }

    pub fn build_call(&mut self, func: LLVMValueRef, args: &mut Vec<LLVMValueRef>) -> LLVMValueRef {
        unsafe {
            LLVMBuildCall(
                self.into(),
                func,
                args.as_mut_ptr(),
                args.len() as c_uint,
                EMPTY_STR.as_ptr() as *const c_char,
            )
        }
    }

    pub fn build_phi(&mut self, ty: LLVMTypeRef) -> LLVMValueRef {
        unsafe { LLVMBuildPhi(self.into(), ty, EMPTY_STR.as_ptr() as *const c_char) }
    }

    pub fn build_ret(&mut self, val: Option<LLVMValueRef>) -> LLVMValueRef {
        unsafe {
            match val {
                Some(x) => LLVMBuildRet(self.into(), x),
                None => LLVMBuildRetVoid(self.into()),
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module(LLVMModuleRef);
native_eq!(Module, LLVMModuleRef);
finalizer!(Module, LLVMDisposeModule);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExecutionEngine(LLVMExecutionEngineRef);
native_eq!(ExecutionEngine, LLVMExecutionEngineRef);
finalizer!(ExecutionEngine, LLVMDisposeExecutionEngine);

pub fn init_native_jit() {
    unsafe {
        LLVM_InitializeNativeAsmPrinter();
        LLVM_InitializeNativeAsmParser();
        LLVM_InitializeNativeTarget();
        LLVMLinkInMCJIT();
    }
}

pub fn generic_value_to_int(value: LLVMGenericValueRef) -> i64 {
    unsafe { LLVMGenericValueToInt(value, LLVM_TRUE) as i64 }
}

impl ExecutionEngine {
    pub fn get_function(&self, name: &str) -> Option<LLVMValueRef> {
        let c_name = CString::new(name).unwrap();
        let function_name = c_name.to_bytes_with_nul().as_ptr() as *const c_char;
        unsafe {
            let mut func: LLVMValueRef = mem::uninitialized();
            if LLVMFindFunction(self.into(), function_name, &mut func) == LLVM_FALSE {
                Some(func)
            } else {
                None
            }
        }
    }

    pub fn remove_module(&mut self, module: &mut Module) -> Result<(), CString> {
        unsafe {
            let mut error = mem::uninitialized();
            if LLVMRemoveModule(self.into(), module.into(), &mut module.0, &mut error) == 1 {
                let message = CStr::from_ptr(error).to_owned();
                LLVMDisposeMessage(error);
                Err(message)
            } else {
                Ok(())
            }
        }
    }

    pub fn run(&self) -> Option<LLVMGenericValueRef> {
        let main_func = self.get_function("main");
        if let Some(func) = main_func {
            unsafe {
                let args = mem::uninitialized();
                Some(LLVMRunFunction(self.into(), func, 0, args))
            }
        } else {
            None
        }
    }
}

impl Module {
    pub fn new(ctx: &Context, name: &str) -> Module {
        let c_name = CString::new(name).unwrap();
        let module_name = c_name.to_bytes_with_nul().as_ptr() as *const c_char;
        unsafe { LLVMModuleCreateWithNameInContext(module_name, ctx.into()).into() }
    }

    pub fn add_function(
        &mut self,
        name: &str,
        args: &mut Vec<LLVMTypeRef>,
        ret: LLVMTypeRef,
        variadic: bool,
    ) -> LLVMValueRef {
        let c_name = CString::new(name).unwrap();
        let fn_name = c_name.to_bytes_with_nul().as_ptr() as *const c_char;
        let fn_ty = function_ty(args, ret, variadic);
        unsafe { LLVMAddFunction(self.into(), fn_name, fn_ty) }
    }

    pub fn get_function(&mut self, name: &str) -> Option<LLVMValueRef> {
        let c_name = CString::new(name).unwrap();
        let func_name = c_name.to_bytes_with_nul().as_ptr() as *const c_char;

        let func = unsafe { LLVMGetNamedFunction(self.into(), func_name) };
        if func.is_null() {
            None
        } else {
            Some(func)
        }
    }

    pub fn to_cstring(&self) -> CString {
        unsafe {
            let llvm_raw_str = LLVMPrintModuleToString(self.into());
            let owned_str = CStr::from_ptr(llvm_raw_str).to_owned();
            LLVMDisposeMessage(llvm_raw_str);
            owned_str
        }
    }

    pub fn verify(&self) -> Result<(), CString> {
        unsafe {
            let mut error = mem::uninitialized();
            let action = LLVMVerifierFailureAction::LLVMReturnStatusAction;
            if LLVMVerifyModule(self.into(), action, &mut error) == 1 {
                let message = CStr::from_ptr(error).to_owned();
                LLVMDisposeMessage(error);
                Err(message)
            } else {
                Ok(())
            }
        }
    }

    pub fn create_execution_engine(&self) -> Result<ExecutionEngine, CString> {
        unsafe {
            let mut engine: LLVMExecutionEngineRef = mem::uninitialized();
            let mut error = mem::uninitialized();
            if LLVMCreateExecutionEngineForModule(&mut engine, self.into(), &mut error) == 1 {
                let message = CStr::from_ptr(error).to_owned();
                LLVMDisposeMessage(error);
                Err(message)
            } else {
                Ok(engine.into())
            }
        }
    }

    pub fn emit_object_file(&self, target: &TargetMachine, filename: &str) -> Result<(), CString> {
        let c_name = CString::new(filename).unwrap();
        let c_filename = c_name.to_bytes_with_nul().as_ptr() as *mut c_char;
        unsafe {
            let file_type = LLVMCodeGenFileType::LLVMObjectFile;
            let mut error = mem::uninitialized();
            if LLVMTargetMachineEmitToFile(
                target.into(),
                self.into(),
                c_filename,
                file_type,
                &mut error,
            ) == 1
            {
                let message = CStr::from_ptr(error).to_owned();
                LLVMDisposeMessage(error);
                Err(message)
            } else {
                Ok(())
            }
        }
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.to_cstring().into_string() {
            Ok(x) => write!(f, "{}", x),
            Err(x) => write!(f, "{}", x.utf8_error()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TargetMachine(LLVMTargetMachineRef);
native_eq!(TargetMachine, LLVMTargetMachineRef);
finalizer!(TargetMachine, LLVMDisposeTargetMachine);

impl TargetMachine {
    pub fn default() -> Result<TargetMachine, CString> {
        unsafe {
            LLVM_InitializeAllTargetInfos();
            LLVM_InitializeAllTargets();
            LLVM_InitializeAllTargetMCs();
            LLVM_InitializeAllAsmParsers();
            LLVM_InitializeAllAsmPrinters();

            let triple = LLVMGetDefaultTargetTriple();

            let mut target = mem::uninitialized();
            let mut error = mem::uninitialized();
            if LLVMGetTargetFromTriple(triple, &mut target, &mut error) == 1 {
                let message = CStr::from_ptr(error).to_owned();
                LLVMDisposeMessage(error);
                return Err(message);
            }

            // generic (x86-64)
            let cpu = CString::new("generic").unwrap();
            let c_cpu_str = cpu.to_bytes_with_nul().as_ptr() as *const c_char;
            let features = CString::new("").unwrap();
            let c_features_str = features.to_bytes_with_nul().as_ptr() as *const c_char;

            let level = LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault;
            let reloc = LLVMRelocMode::LLVMRelocDefault;
            let code_model = LLVMCodeModel::LLVMCodeModelDefault;
            let machine = LLVMCreateTargetMachine(
                target,
                triple,
                c_cpu_str,
                c_features_str,
                level,
                reloc,
                code_model,
            );
            Ok(TargetMachine(machine))
        }
    }
}

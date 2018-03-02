//! Useful macro definitions for LLVM
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

/// Implements seamless conversion between the llvm type and the rust equivalent.
macro_rules! native_eq {
    ($name:ident, $alias:ty) => (
        impl From<$alias> for $name {
            fn from(x: $alias) -> $name {
                $name(x)
            }
        }

        impl<'a> From<&'a $name> for $alias {
            fn from(x: &'a $name) -> $alias {
                x.0
            }
        }

        impl<'a> From<&'a mut $name> for $alias {
            fn from(x: &'a mut $name) -> $alias {
                x.0
            }
        }
    )
}

/// Automatically frees the element with the disposing function.
macro_rules! finalizer {
    ($name:ident, $func:ident) => (
        impl Drop for $name {
            fn drop(&mut self) {
                unsafe { $func(self.into()) }
            }
        }
    )
}

macro_rules! unary_op {
    ($name:ident, $func:ident) => (
        pub fn $name(&mut self, val: LLVMValueRef) -> LLVMValueRef {
            unsafe { $func(self.into(), val, EMPTY_STR.as_ptr() as *const c_char) }
        }
    );
    ($name:ident, $ifunc:ident, $ffunc:ident) => (
        pub fn $name(&mut self, val: LLVMValueRef, float: bool) -> LLVMValueRef {
            unsafe {
                (if float { $ffunc } else { $ifunc })(self.into(),
                    val,
                    EMPTY_STR.as_ptr() as *const c_char)
            }
        }
    )
}

macro_rules! bin_op {
    ($name:ident, $func:ident) => (
        pub fn $name(&self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
            unsafe { $func(self.into(), lhs, rhs, EMPTY_STR.as_ptr() as *const c_char) }.into()
        }
    );
    ($name:ident, $ifunc:ident, $ffunc:ident) => (
        pub fn $name(&self, lhs: LLVMValueRef, rhs: LLVMValueRef, float: bool) -> LLVMValueRef {
            unsafe {
                (if float { $ffunc } else { $ifunc })(self.into(),
                    lhs,
                    rhs,
                    EMPTY_STR.as_ptr() as *const c_char)
            }
        }
    )
}

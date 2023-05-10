use crate::chunk::{Chunk, OpCode};
use crate::vm::VM;

mod chunk;
mod value;
mod vm;

fn main() {
  let mut vm = VM::new();

  let mut chunk = Chunk::new();

  let constant = chunk.add_constant(1.2);
  chunk.write_opcode(OpCode::OpConstant, 123);
  chunk.write(constant as u8, 123);

  chunk.write_opcode(OpCode::OpReturn, 123);
  chunk.disassemble("test chunk");

  vm.interpret(&chunk);

  chunk.free();
  vm.free();
}

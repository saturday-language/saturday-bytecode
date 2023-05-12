use crate::chunk::{Chunk, OpCode};
use crate::vm::VM;

mod chunk;
mod value;
mod vm;

fn main() {
  let mut vm = VM::new();

  let mut chunk = Chunk::new();

  let constant = chunk.add_constant(1.2);
  chunk.write_opcode(OpCode::Constant, 123);
  chunk.write(constant, 123);

  let constant = chunk.add_constant(3.4);
  chunk.write_opcode(OpCode::Constant, 123);
  chunk.write(constant, 123);

  chunk.write_opcode(OpCode::Add, 123);

  let constant = chunk.add_constant(5.6);
  chunk.write_opcode(OpCode::Constant, 123);
  chunk.write(constant, 123);

  chunk.write_opcode(OpCode::Divide, 123);
  chunk.write_opcode(OpCode::Negate, 123);

  chunk.write_opcode(OpCode::Return, 123);
  chunk.disassemble("test chunk");

  vm.interpret(&chunk);

  chunk.free();
  vm.free();
}

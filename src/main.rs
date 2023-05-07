use crate::chunk::{Chunk, OpCode};

mod chunk;
mod value;

fn main() {
  let mut chunk = Chunk::new();

  let constant = chunk.add_constant(1.2);
  chunk.write_opcode(OpCode::OpConstant);
  chunk.write(constant as u8);

  chunk.write_opcode(OpCode::OpReturn);
  chunk.disassemble("0000");
  chunk.free();
}

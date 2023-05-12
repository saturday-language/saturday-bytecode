use std::env::args;
use std::io;
use std::io::{BufRead, stdout, Write};
use crate::chunk::{Chunk, OpCode};
use crate::vm::{InterpretResult, VM};

mod chunk;
mod value;
mod vm;

fn main() {
  let args: Vec<String> = args().collect();
  let mut vm = VM::new();

  match args.len() {
    1 => repl(&mut vm),
    2 => run_file(&mut vm, &args[1]).expect("Could not run file"),
    _ => {
      println!("Usage: saturday-bytecode [script]");
      std::process::exit(64);
    }
  }

  vm.free();
}

fn repl(vm: &mut VM) {
  let stdin = io::stdin();
  print!("> ");
  stdout().flush().expect("flush error");
  for line in stdin.lock().lines() {
    if let Ok(line) = line {
      if line.is_empty() {
        break;
      }

      let _ = vm.interpret(&line);
    } else {
      break;
    }

    print!("> ");
    stdout().flush().expect("flush error");
  }
}

fn run_file(vm: &mut VM, path: &str) -> io::Result<()> {
  let buf = std::fs::read_to_string(path)?;
  match vm.interpret(&buf) {
    InterpretResult::CompileError => std::process::exit(65),
    InterpretResult::RuntimeError => std::process::exit(70),
    InterpretResult::Ok => std::process::exit(0),
  }

  Ok(())
}

# k-lox-jit
Read the [report](Computer%20Architecture%20Honors%20Contract%20in%20Just-In-Time%20Compilers.pdf) for a summary of the project.

To run the project, you will the need:
- the `rust` toolchain installed. You can install it using `rustup` by following the instructions [here](https://www.rust-lang.org/tools/install)
- the `qemu` emulator for aarch64. You can install it and other dependencies using the following command:
```bash
sudo apt install qemu-user qemu-user-static gcc-aarch64-linux-gnu binutils-aarch64-linux-gnu binutils-aarch64-linux-gnu-dbg build-essential gdb-multiarch
```

# Running the example file
```bash
cargo run test.klox
```

# Debugging with GDB
```bash
# the -g 1234 option will make qemu wait for gdb to connect at port 1234 before executing
cargo build -q && qemu-aarch64 -g 1234 -L /usr/aarch64-linux-gnu target/aarch64-unknown-linux-gnu/debug/k-lox-jit test.klox

# run gdb with the executable name, and run a command to connect to the qemu instance using -ex
gdb-multiarch -q --se=target/aarch64-unknown-linux-gnu/debug/lox-jit -ex 'set architecture aarch64' -ex 'target remote localhost:1234'
```

### Roadmap:
- Register allocation for JITter
- Interpreter for bytecode (to switch between JIT and interpreter)
- Look into making on-stack replacement (OSR) possible
1. Types
2. Objects
3. Closures
4. Garbage collector
5. Async/await runtime (event loop)

### Notes from Vihaan
// neutral and absorbing elements in arithmatic

### Notes from Mikail
- calculate stack size
- can take max size of blocks and preallocate memory for stack
- think about supporting variable length stack allocation (int a[n])

### Notes from Tom
- declared variable reordering can be performed at end of parsing block/function
- so declared variables are at base and VLA are on top
- multiple VLA through storing pointers 

### Notes from Yueyang Tang
- implement simply typed lambda calculus, and JIT compile that
- first: STLC (first level, this is where you do check types) JIT compile into imperative program 
- erase all types, then JIT compile
- lifting variables out of closures for compiling basic blocks

- Syntax directed translation: directly translate 
- Futumura projection: partial evaluation
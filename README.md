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
// calculate stack size
// can take max size of blocks and preallocate memory for stack
// think about supporting variable length stack allocation (int a[n])

### Notes from Tom
// declared variable reordering can be performed at end of parsing block/function
// so declared variables are at base and VLA are on top
// multiple VLA through storing pointers 

### Notes from Yueyang Tang
// implement simply typed lambda calculus, and JIT compile that
// first: STLC (first level, this is where you do check types) JIT compile into imperative program 
// erase all types, then JIT compile
// lifting variables out of closures for compiling basic blocks

// Syntax directed translation: directly translate 
// Futumura projection: partial evaluation

# Running the example file
```bash
cargo run test.lox
```

# How to debug generated assembly
```bash
cargo test
# cargo test will fail, and provide the name of the executable
# example command:
qemu-aarch64 -g 1234 -L /usr/aarch64-linux-gnu target/aarch64-unknown-linux-gnu/debug/lox-jit test.lox
# the -g 1234 option will make qemu wait for gdb to connect at port 1234 before executing
# run gdb with the executable name, and run a command to connect to the qemu instance using -ex
gdb-multiarch -q --se=/home/desmond-lin-7/cs250/lox-jit/target/aarch64-unknown-linux-gnu/debug/lox-jit -ex 'set architecture aarch64' -ex 'target remote localhost:1234'
```
In gdb,
```
layout asm
x/16x $sp for top sixteen words on stack
```

# How to assemble and execute ARM assembly
```bash
aarch64-linux-gnu-gcc
```
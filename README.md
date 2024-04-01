### Tasks
loop unrolling in bytecode
variables, loops (jump instruction)

# Running the example file
```bash
cargo build --quiet && qemu-aarch64 -E RUST_LOG=trace -L /usr/aarch64-linux-gnu target/aarch64-unknown-linux-gnu/debug/lox-jit test.lox
```

# How to debug generated assembly
```bash
cargo test
# cargo test will fail, and provide the name of the executable
# example command:
qemu-aarch64 -L /usr/aarch64-linux-gnu -g 1234 /home/desmond-lin-7/cs250/lox-jit/target/aarch64-unknown-linux-gnu/debug/deps/lox_jit-3d3658d22163a622 'jit::jit_tests::test_jit2' --exact --nocapture
# the -g 1234 option will make qemu wait for gdb to connect at port 1234 before executing
# run gdb with the executable name, and run a command to connect to the qemu instance using -ex
gdb-multiarch -q /home/desmond-lin-7/cs250/lox-jit/target/aarch64-unknown-linux-gnu/debug/deps/lox_jit-3d3658d22163a622 -ex 'target remote :1234'
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
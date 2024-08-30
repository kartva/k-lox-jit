## Calling Conventions
(Procedure Call Standard for the Arm® 64-bit Architecture (AArch64))
![[ARM Registers.png]]
Stack pointer should be 16-byte aligned at all times. (`SP mod 16 = 0`)
### Frame pointers
Form a linked-list of previous frame pointers.
- Frame pointer points to start of previous function stack.
- When a new function is called, we store the frame pointer and link register onto the stack. We then set the frame pointer to point to the location of the saved frame pointer.
- The frame pointer points to the start of the new function, and the last frame pointer.
### Returning
Link register stores address to jump to when encountering `ret`. `bl` instruction stores `ip + 8` (next instruction) into `lr` and then jumps to new function start. Since `bl` overwrites `lr`, the calling function needs to store `lr` given from parent into stack at the start of the function, and restore `lr` from the stack at the end before calling `ret`.
## Load and Store
### Load
#### LDR (Load Register)
##### Literal form
`LDR <Rd> <label>`
Calculates an address from the PC value and an immediate offset, loads a word from memory, and writes it to a register.
##### Immediate form
Load Register (immediate) loads a word or doubleword from memory and writes it to a register. The address that is used for the load is calculated from a base register and an immediate offset.
###### Post-index
`LDR <Xt>, [<Xn|SP>], #<simm>` loads `Xn` address value into `Xt`, then sets `Xn = Xn + offset`.
###### Pre-index
`LDR <Xt>, [<Xn|SP>, #<simm>]!` loads `Xn + offset` address value into `Xt`, then sets `Xn = Xn + offset`. That is, the offset calculation is performed before loading, hence pre-index.
###### Unsigned offset
`LDR <Xt>, [<Xn|SP>{, #<pimm>}]` (`pimm` is positional positive immediate byte offset)
##### Register form
`LDR <Xt>, [<Xn|SP>, (<Wm>|<Xm>){, <extend> {<amount>}}]`
Load Register (register) calculates an address from a base register value and an offset register value, loads a word from memory, and writes it to a register. The offset register value can optionally be shifted and extended.
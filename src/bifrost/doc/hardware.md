# Control machine architectures

## Things that are constant (except in Strawberry, see below)
- One flop per variable.
- Each bite of the program becomes a state in an FSM (note: the FSM may also have other states).
- Variable flops enabled via the signal `update` (definition varies).

## Vanilla
Four-phase hardware. IDLE -> [bite states] -> DONE -> IDLE. Input variables and main's return address are initialized when transitioning from IDLE to the entry bite's state. The `update` signal for the variable flops is defines as `start | advance`, the former capturing the IDLE -> ... transition, and the latter capturing transitions from bite states (including the final transition to DONE).

## Chocolate
Both two and four-phase hardware variants. [bite states] -> DONE -> [bite states]. Rather than having a dedicated IDLE state, this idles in the entry bite's state. Both the action 'want' signals and the 'update' signal are predicated on the 'running' signal. Rather than initializing the input variables (and main's return address), we bypass their flops with muxes that tap directly into the input wires when a signal 'initial' is high.

## Strawberry
Combinational architecture; no FSM and no flops.
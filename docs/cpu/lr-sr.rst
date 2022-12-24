Load-reserve/store-release implementation
=========================================

These synchronization primitives are chosen over other variants due to AXI4 having direct support for them through its `exclusive access <https://developer.arm.com/documentation/102202/0200/Atomic-accesses>`_ transactions.

This pushes the burden of actually making things work to the memory controller(s) but hopefully there's support for it there already. If not, the following can be done:

- Let's have a BRAM in 8kx1-bit config. The BRAM address is a 13-bit hash of the transaction address.
- The data is a single 'valid' bit.

Operation:

- On exclusive load, the valid bit is set for the corresponding address.
- On exclusive store, the valid bit is checked and the store is cancelled if the bit is clear. Then, the valid bit is cleared.

If there are multiple ports to a memory, each port will have to have a copy of the above mechanism for each port. That is to say, the number of BRAMs needed is the number of ports *squared*.

Each exclusive load on any port sets all the valid bits in the BRAMs for that port.
Each exclusive store does three things:
#. checks if all the bits are set in the BRAMs for that port. If any is cleared, the write fails.
#. simultaneously, clears the valid bit on all *other* ports' BRAMs that are assigned to this port
#. clears the valid bit in the BRAMs (maybe one of them is sufficient) for this port

This protocol I think leaves one cycle uncertainty, that needs to be checked: if a write to the same address happens in the same cycle on two or more ports, they could still sneak through. This can be avoided if the BRAMs are operated in 'read-new-value' conflict resolution mode.

store-release needs to return a result code, which comes with the write-response on AXI4. Thus, store-release stalls until the write-response, in effect, flushing the write queue.

With this, an atomic increment would look something like this::

  retry:
      $rD <- ! MEM[$rA,<ofs>]
      $rD <- $rD + 1
      $rD <- ? MEM[$rA,<ofs>] <- $rD
      if $rD != 0 $pc <- retry # to mimic RiscV behavior, we signal success with 0.

More complex primitives, can be built out of this simple one. Many architectures add things like atomic increment as a single instruction, the idea being that one can avoid all the traffic between the processor and the memory controller if these primitives are implemented right there. I don't want to go that far, so I don't have any such primitives.

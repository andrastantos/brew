
CSRs
====

Control and Status registers (CSRs) provide configuration and status information for a processor. This address space is unique to every processor and thread in the system, while some of the underlying registers may be shared amongst them.

The address space uses a 16-bit address, each address corresponding to a 32-bit value. Most addresses are not used. The MSB of the address is always set for CSR accesses issued from TASK mode. In SCHEDULER-mode, all 16-bits of the address are under SW control; this allows for arranging for sensitive CSRs to be only accessible from SCHEDULER-mode, albeit at the cost of more complicated address layout.

Implementations can provide more fine-grained access control through implementation-defined means so long as they maintain full access to the CSR space from SCHEDULER-mode.

CSR registers
-------------

Currently the following registers are defined:

========== ============== ============== ===================================================
Address    Name           Access type    Description
========== ============== ============== ===================================================
0x0000     ECAUSE         R              Exception cause register
0x0001     EADDR          R              Exception address register
0x0100     DIRTY          RW             Register dirty mask
0x0101     VSTART         RW             Vector operation start in bytes
0x0102     VEND           RW             Vector operation end in bytes
0x8000     MARCH          R              Machine architecture and version register
0x8001     CAPABILITY     R              Capability bit-field
0x8010     VLEN           R              HW vector length in bytes
0x8020     FPSTAT         RW             Floating-point accumulated status and control register
========== ============== ============== ===================================================


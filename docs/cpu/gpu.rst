Appendix E: GPU variant
=======================

A variant of the Brew ISA can be used to implement GPUs as well.

The essence of GPU workloads is that the same (relatively short) computation needs to be repeated thousands upon thousands of times on different data, and - crucially - those iterations don't have dependencies between themselves (no loop-carried dependencies in compiler-parlor). This algorithm in GPU-speak is a kernel. The construction of an image is done though several such kernels stringed together into a data-flow algorithm.

The consequence of this kind of workload is that we don't care about individual instruction latency; what we care much more about is to make sure the processor is doing useful work every cycle. So, instead of optimizing for minimal latency (by adding branch-prediction for instance) we optimize for overhead. We will implement simple, but heavily hyper-threaded cores. These many execution contexts help hide the latency of the instructions, in fact, help with resolving hazards in the system without (too much) stalling as well.

Simplifications could include:
- No HW-managed caches
- No branch-predictor
- No forwarding paths

New features:
- Many execution contexts (maybe 8 threads per core)
- TCMs (to optimize the otherwise enormous DRAM bandwidth)

Of course a true GPU is much more than this: it contains a ton of special HW for things that are best done in accelerators, such as texture mapping engines and video generation. The instruction set is also specialized. From what I gather, it's normally of the VLIW ilk. A vector engine with many many specialized instructions for the workload at hand.

There have been attempts to use general purpose ISAs (Intels `Larabee <https://en.wikipedia.org/wiki/Larrabee_(microarchitecture)>`_), they usually weren't successful. Of course we're not after a competitive advantage here: any GPU implementation, in an FPGA, based on the Brew ISA would be woefully underpowered compared to anything out there. However it would be a fun project and a good learning experience.

Below are some notes on the particularities of such a use-case:

Synchronization
---------------

GPUs have a *ton* of synchronization operations, but I don't think any of them are *really* necessary. The LR/SR primitives with controlled cache-invalidation and some TCMs for quick access should suffice and would allow to emulate anything we might want.

At any rate, some material on the subject:
https://gpuopen.com/gdc-presentations/2019/gdc-2019-agtd5-breaking-down-barriers.pdf
https://mynameismjp.wordpress.com/2018/03/06/breaking-down-barriers-part-1-whats-a-barrier/ - write-up of the same slide-deck

Barriers
--------

Barriers are used between kernels (if needed). The idea is that one kernel should completely finish before another can start. This is at the level of the thread scheduler, but I think it still can be implemented, using LR/SR quite easily:

Let's say that Kernel B depends on Kernel A, so none of Kernel Bs threads can start before all of Kernel As threads completed.

#. The thread scheduler writes the number of threads in Kernel A into a memory location. Call this 'active thread count'
#. The thread scheduler starts scheduling threads of Kernel A
#. At the end of each thread of Kernel A, an atomic decrement is performed on the active thread count
#. The thread scheduler doesn't schedule any threads of Kernel B, until Kernel As active thread counter is non-0.

This allows for more complex dependency graphs as well as long as each kernel has an individual counter like that. It also allows the scheduler to keep scheduling threads from independent threads as long as there is work to be done and still observe the dependencies.

AMD notes
---------

AMD actually documents their `ISA <https://developer.amd.com/wp-content/resources/RDNA2_Shader_ISA_November2020.pdf
https://gpuopen.com/amd-isa-documentation/>`_

There are a lot of data types (especially for textures), and a lot of memory types. There seems to be a TCM (they call it LDS). They also seem to indicate that L1 caches are write-through (thus the benefit of TCM in data-sharing). LDS is shared within a 'work group'. A work-group is whatever is executing on up to 4 SIMD engines, where each SIMD engine can have up to 32 workitems (thread, maybe?). I don't see any SMT features on a per CU level, but there is something about it in the 'dispatch processor'. So, the 32 workitems then must be the SIMT lanes. Vector registers are 32-bit wide, which would mean that they are per-lane. There seems to be 256 addressable vector registers and 106 scalar registers (these are NOT per lane). All of these are 32-bit wide.

There are sub-vector data-types (16-bit, two-lanes-in-one-reg) variants.

There is some confusion about register counts in the block diagram and the ISA. That might have something to do with SMT. If so, there seems to be 4-way SMT capability in the CUs.

The instruction set is mainly a 32-bit one, but with lots of 64-bit or even longer instructions, especially for vector ops. Some are even 128+bits long!

There are 16 atomic instructions (add/sub...)

Page 91 contains a good diagram of the memory hierarchy.

This thing is anything but simple. There are a *TON* of instructions.

More on synchronization: there is an S_SLEEP and S_WAKEUP pair.

S_BARRIER - thread-group thingy. There is some verbiage around non-created and terminated waves.
S_ICACHE_INV - invalidates the whole ICache
S_DCACHE_INV - invalidates the whole (scalar) L0 DCache
S_GL1_INV    - invalidates the GL1 cache whatever that is

NOTE: there's no flush. That seems to indicate:

* write-through
* non-coherent

nVidia notes
------------

A good list of instructions supported by CUDA cores:

https://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html#arithmetic-instructions__throughput-native-arithmetic-instructions

* FP16X2 lane-extract
* Widening/narrowing/lane-extract for fixed point types is easy with the lane-swizzle operation
* maybe lane-swizzle with dynamic sizzle (binary op)?


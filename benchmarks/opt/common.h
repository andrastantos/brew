/* Shared helpers for the code-generation test cases in this directory.

   Every test is one C file whose main() runs a small kernel on data
   generated from a volatile seed (so the compiler can't fold the work
   away) and returns 0 when its checksum matches the expected value,
   1 otherwise.  Nothing is printed, so the measured instructions are
   the kernel's plus a fixed startup cost (see empty.c).

   Built with -DOPT_PRINT (on the host or on Brew), main() prints the
   checksum instead; that is how the expected values were obtained.
   Use only fixed-width types: the tests must give the same result on
   a 64-bit host.  */

#ifndef OPT_COMMON_H
#define OPT_COMMON_H

#include <stdint.h>
#include <stddef.h>

static volatile uint32_t opt_seed = 0x2545f491u;
static volatile uint32_t opt_sink;

/* xorshift32: cheap, deterministic, never 0 for a non-zero seed.  */
static inline uint32_t
opt_rand (uint32_t *s)
{
  uint32_t x = *s;
  x ^= x << 13;
  x ^= x >> 17;
  x ^= x << 5;
  return *s = x;
}

/* Mix a value into a running checksum.  */
static inline uint32_t
opt_mix (uint32_t h, uint32_t v)
{
  return (h ^ v) * 0x01000193u;
}

#define OPT_NOINLINE __attribute__ ((noinline))

#ifdef OPT_PRINT
#include <stdio.h>
#define OPT_RESULT(v, expected)                                         \
  do { printf ("0x%08xu\n", (unsigned) (v)); return 0; } while (0)
#else
#define OPT_RESULT(v, expected)                                         \
  do { uint32_t r_ = (v); opt_sink = r_; return r_ != (expected); } while (0)
#endif

#endif /* OPT_COMMON_H */

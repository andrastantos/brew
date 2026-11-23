/* Bit manipulation: popcount, clz, ctz, parity, bit reversal and
   rotates, both as builtins and as hand-written loops.  */
#include "common.h"

#define N 1500

static OPT_NOINLINE uint32_t
pop_loop (uint32_t x)
{
  uint32_t n = 0;
  while (x)
    {
      x &= x - 1;
      n++;
    }
  return n;
}

static OPT_NOINLINE uint32_t
reverse (uint32_t x)
{
  x = ((x >> 1) & 0x55555555u) | ((x & 0x55555555u) << 1);
  x = ((x >> 2) & 0x33333333u) | ((x & 0x33333333u) << 2);
  x = ((x >> 4) & 0x0f0f0f0fu) | ((x & 0x0f0f0f0fu) << 4);
  x = ((x >> 8) & 0x00ff00ffu) | ((x & 0x00ff00ffu) << 8);
  return (x >> 16) | (x << 16);
}

static inline uint32_t
rotl (uint32_t x, unsigned r)
{
  return (x << (r & 31)) | (x >> (-r & 31));
}

int
main (void)
{
  uint32_t s = opt_seed, h = 0;
  for (int i = 0; i < N; i++)
    {
      uint32_t x = opt_rand (&s) >> (i & 15);
      h = opt_mix (h, (uint32_t) __builtin_popcount (x));
      h = opt_mix (h, pop_loop (x));
      h = opt_mix (h, x ? (uint32_t) __builtin_clz (x) : 32);
      h = opt_mix (h, x ? (uint32_t) __builtin_ctz (x) : 32);
      h = opt_mix (h, (uint32_t) __builtin_parity (x));
      h = opt_mix (h, reverse (x));
      h = opt_mix (h, rotl (x, i));
    }
  OPT_RESULT (h, 0xe3212bc1u);
}

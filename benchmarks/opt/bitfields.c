/* Bit-field packing and unpacking, narrow struct members (8/16-bit
   loads and stores with zero/sign extension) and flag tests.  */
#include "common.h"

#define N 1500

struct pkt
{
  uint32_t ver : 3;
  uint32_t kind : 5;
  int32_t delta : 12;
  uint32_t urgent : 1;
  uint32_t len : 11;
  int8_t prio;
  uint8_t flags;
  int16_t offset;
};

static struct pkt pk[N];

static OPT_NOINLINE uint32_t
process (struct pkt *p, int n)
{
  uint32_t h = 0;
  for (int i = 0; i < n; i++)
    {
      if (p[i].urgent && p[i].ver == 4)
        p[i].len += 1;
      if (p[i].flags & 0x10)
        p[i].delta = -p[i].delta;
      p[i].kind = (p[i].kind + p[i].prio) & 31;
      h = opt_mix (h, (uint32_t) (p[i].delta + p[i].offset) ^ p[i].len ^ p[i].kind);
    }
  return h;
}

int
main (void)
{
  uint32_t s = opt_seed;
  for (int i = 0; i < N; i++)
    {
      uint32_t r = opt_rand (&s), q = opt_rand (&s);
      pk[i].ver = r;
      pk[i].kind = r >> 3;
      pk[i].delta = (int32_t) (r >> 8) % 2000;
      pk[i].urgent = r >> 20;
      pk[i].len = r >> 21;
      pk[i].prio = (int8_t) q;
      pk[i].flags = q >> 8;
      pk[i].offset = (int16_t) (q >> 16);
    }
  OPT_RESULT (process (pk, N), 0x28ab3eadu);
}

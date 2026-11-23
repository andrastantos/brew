/* Library memcpy/memset/memcmp at small and medium sizes, and struct
   assignment (which GCC expands inline or as a memcpy call).  */
#include <string.h>
#include "common.h"

#define N 4096

struct rec { uint32_t id; uint16_t a, b; uint8_t tag[12]; uint32_t v[4]; };

static uint8_t src[N], dst[N];
static struct rec recs[64], copy[64];

int
main (void)
{
  uint32_t s = opt_seed, h = 0;
  for (int i = 0; i < N; i++)
    src[i] = opt_rand (&s);
  for (int i = 0; i < 64; i++)
    {
      recs[i].id = opt_rand (&s);
      recs[i].a = opt_rand (&s);
      recs[i].b = opt_rand (&s);
      for (int k = 0; k < 4; k++)
        recs[i].v[k] = opt_rand (&s);
      memset (recs[i].tag, i, sizeof recs[i].tag);
    }
  for (int round = 0; round < 8; round++)
    {
      for (size_t len = 1; len <= 64; len++)
        {
          size_t off = (len * 37 + round) % (N - 64);
          memcpy (dst + off, src + off + (len & 3), len);
          int c = memcmp (dst + off, src + off, len);
          h = opt_mix (h, (uint32_t) ((c > 0) - (c < 0)));
        }
      memcpy (dst, src, N);
      memset (dst + round * 100, round, 300);
      h = opt_mix (h, dst[round * 101]);
      for (int i = 0; i < 64; i++)
        copy[(i + round) & 63] = recs[i];
      h = opt_mix (h, copy[round].id ^ copy[round].v[3]);
    }
  OPT_RESULT (h, 0xe3ade0e2u);
}

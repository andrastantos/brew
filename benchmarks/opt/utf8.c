/* UTF-8 decoding: a branch on the lead byte's class (1 to 4 bytes),
   continuation-byte checks, a mostly-ASCII and a mixed input.  */
#include "common.h"

#define N 4000

static uint8_t buf[N + 4];

static size_t
put (uint8_t *p, uint32_t c)
{
  if (c < 0x80) { p[0] = c; return 1; }
  if (c < 0x800) { p[0] = 0xc0 | c >> 6; p[1] = 0x80 | (c & 63); return 2; }
  if (c < 0x10000) { p[0] = 0xe0 | c >> 12; p[1] = 0x80 | (c >> 6 & 63); p[2] = 0x80 | (c & 63); return 3; }
  p[0] = 0xf0 | c >> 18; p[1] = 0x80 | (c >> 12 & 63); p[2] = 0x80 | (c >> 6 & 63); p[3] = 0x80 | (c & 63);
  return 4;
}

static OPT_NOINLINE uint32_t
decode (const uint8_t *p, const uint8_t *end, uint32_t *count)
{
  uint32_t h = 0, n = 0;
  while (p < end)
    {
      uint32_t c = *p++;
      int extra;
      if (c < 0x80)
        extra = 0;
      else if ((c & 0xe0) == 0xc0)
        extra = 1, c &= 0x1f;
      else if ((c & 0xf0) == 0xe0)
        extra = 2, c &= 0x0f;
      else if ((c & 0xf8) == 0xf0)
        extra = 3, c &= 0x07;
      else
        {
          c = 0xfffd;
          extra = 0;
        }
      while (extra-- && p < end && (*p & 0xc0) == 0x80)
        c = c << 6 | (*p++ & 63);
      h = opt_mix (h, c);
      n++;
    }
  *count = n;
  return h;
}

static uint32_t
run (uint32_t *s, uint32_t ascii_pct)
{
  size_t len = 0;
  while (len < N)
    {
      uint32_t r = opt_rand (s), c;
      if (r % 100 < ascii_pct)
        c = 0x20 + (r >> 8) % 95;
      else
        switch ((r >> 8) & 3)
          {
          case 0: c = 0x80 + (r >> 10) % 0x780; break;
          case 1: c = 0x800 + (r >> 10) % 0xd000; break;
          case 2: c = 0x10000 + (r >> 10) % 0x10000; break;
          default: c = 0x41; break;
          }
      len += put (buf + len, c);
    }
  uint32_t count, h = decode (buf, buf + len, &count);
  return opt_mix (h, count);
}

int
main (void)
{
  uint32_t s = opt_seed;
  uint32_t h = run (&s, 95);
  h = opt_mix (h, run (&s, 30));
  OPT_RESULT (h, 0xbd23d72eu);
}

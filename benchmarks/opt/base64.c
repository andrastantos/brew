/* Base64 encode and decode: table lookups, shifts and masks on
   3-byte groups, a reverse table with an invalid-character check.  */
#include "common.h"

#define N 3000

static uint8_t data[N], back[N];
static char text[(N + 2) / 3 * 4 + 1];
static int8_t rev[256];
static const char alpha[] =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

static OPT_NOINLINE size_t
encode (const uint8_t *p, size_t n, char *o)
{
  char *start = o;
  size_t i = 0;
  for (; i + 3 <= n; i += 3)
    {
      uint32_t v = (uint32_t) p[i] << 16 | (uint32_t) p[i + 1] << 8 | p[i + 2];
      *o++ = alpha[v >> 18];
      *o++ = alpha[(v >> 12) & 63];
      *o++ = alpha[(v >> 6) & 63];
      *o++ = alpha[v & 63];
    }
  if (i < n)
    {
      uint32_t v = (uint32_t) p[i] << 16 | (i + 1 < n ? (uint32_t) p[i + 1] << 8 : 0);
      *o++ = alpha[v >> 18];
      *o++ = alpha[(v >> 12) & 63];
      *o++ = i + 1 < n ? alpha[(v >> 6) & 63] : '=';
      *o++ = '=';
    }
  *o = 0;
  return o - start;
}

static OPT_NOINLINE int
decode (const char *t, uint8_t *o)
{
  int n = 0;
  uint32_t acc = 0;
  int bits = 0;
  for (; *t && *t != '='; t++)
    {
      int v = rev[(uint8_t) *t];
      if (v < 0)
        return -1;
      acc = acc << 6 | v;
      bits += 6;
      if (bits >= 8)
        {
          bits -= 8;
          o[n++] = acc >> bits;
        }
    }
  return n;
}

int
main (void)
{
  uint32_t s = opt_seed, h = 0;
  for (int i = 0; i < 256; i++)
    rev[i] = -1;
  for (int i = 0; i < 64; i++)
    rev[(uint8_t) alpha[i]] = i;
  for (int i = 0; i < N; i++)
    data[i] = opt_rand (&s);
  size_t len = encode (data, N - 1, text);
  int n = decode (text, back);
  h = opt_mix (h, (uint32_t) len);
  h = opt_mix (h, (uint32_t) n);
  for (int i = 0; i < n; i++)
    h = opt_mix (h, back[i] ^ data[i]);
  OPT_RESULT (h, 0xc6cefc77u);
}

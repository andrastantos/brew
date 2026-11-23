/* Table-driven CRC-32: byte loads, table indexing, shift/xor chains;
   the table is built at run time (nested loops with a conditional).  */
#include "common.h"

#define N 8192

static uint8_t buf[N];
static uint32_t table[256];

static OPT_NOINLINE void
make_table (void)
{
  for (uint32_t i = 0; i < 256; i++)
    {
      uint32_t c = i;
      for (int k = 0; k < 8; k++)
        c = (c & 1) ? (c >> 1) ^ 0xedb88320u : c >> 1;
      table[i] = c;
    }
}

static OPT_NOINLINE uint32_t
crc32 (const uint8_t *p, size_t n)
{
  uint32_t crc = 0xffffffffu;
  for (size_t i = 0; i < n; i++)
    crc = table[(crc ^ p[i]) & 0xff] ^ (crc >> 8);
  return ~crc;
}

int
main (void)
{
  uint32_t s = opt_seed;
  for (int i = 0; i < N; i++)
    buf[i] = opt_rand (&s);
  make_table ();
  OPT_RESULT (crc32 (buf, N), 0x6dda365eu);
}

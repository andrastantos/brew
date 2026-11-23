/* Bitwise CRC-32: a data-dependent conditional XOR in the inner loop
   (branch versus mask-and-xor), shifts, a fixed-count loop.  */
#include "common.h"

#define N 2048

static uint8_t buf[N];

static OPT_NOINLINE uint32_t
crc32 (const uint8_t *p, size_t n)
{
  uint32_t crc = 0xffffffffu;
  while (n--)
    {
      crc ^= *p++;
      for (int k = 0; k < 8; k++)
        if (crc & 1)
          crc = (crc >> 1) ^ 0xedb88320u;
        else
          crc >>= 1;
    }
  return ~crc;
}

int
main (void)
{
  uint32_t s = opt_seed;
  for (int i = 0; i < N; i++)
    buf[i] = opt_rand (&s);
  OPT_RESULT (crc32 (buf, N), 0xd0871f11u);
}

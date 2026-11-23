/* String hashing (FNV-1a) into an open-addressing table with linear
   probing: inserts and lookups, string compares on collisions.  */
#include <string.h>
#include "common.h"

#define NKEYS 600
#define TSIZE 1024
#define KL 12

static char keys[NKEYS][KL];
static const char *table[TSIZE];
static uint32_t values[TSIZE];

static uint32_t
fnv1a (const char *s)
{
  uint32_t h = 0x811c9dc5u;
  while (*s)
    h = (h ^ (uint8_t) *s++) * 0x01000193u;
  return h;
}

static OPT_NOINLINE uint32_t *
lookup (const char *k, int insert)
{
  uint32_t i = fnv1a (k) & (TSIZE - 1);
  while (table[i])
    {
      if (strcmp (table[i], k) == 0)
        return &values[i];
      i = (i + 1) & (TSIZE - 1);
    }
  if (!insert)
    return 0;
  table[i] = k;
  return &values[i];
}

int
main (void)
{
  uint32_t s = opt_seed, h = 0;
  for (int k = 0; k < NKEYS; k++)
    {
      int len = 3 + opt_rand (&s) % (KL - 4);
      for (int i = 0; i < len; i++)
        keys[k][i] = 'a' + opt_rand (&s) % 4;
      keys[k][len] = 0;
    }
  for (int k = 0; k < NKEYS; k++)
    ++*lookup (keys[k], 1);
  for (int k = 0; k < NKEYS; k++)
    {
      char probe[KL];
      strcpy (probe, keys[(k * 13) % NKEYS]);
      probe[0] ^= k & 1;
      uint32_t *v = lookup (probe, 0);
      h = opt_mix (h, v ? *v : 0xdead);
    }
  OPT_RESULT (h, 0x9ef00dc2u);
}

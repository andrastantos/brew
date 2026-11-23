/* Unbalanced binary search tree: iterative insert, recursive
   traversal and height (call/return overhead, prologues).  */
#include "common.h"

#define N 1000

struct tnode { struct tnode *l, *r; uint32_t key; };

static struct tnode pool[N];

static OPT_NOINLINE void
insert (struct tnode **root, struct tnode *n)
{
  while (*root)
    root = n->key < (*root)->key ? &(*root)->l : &(*root)->r;
  *root = n;
}

static OPT_NOINLINE uint32_t
walk (const struct tnode *t, uint32_t h)
{
  if (!t)
    return h;
  h = walk (t->l, h);
  h = opt_mix (h, t->key);
  return walk (t->r, h);
}

static OPT_NOINLINE int
height (const struct tnode *t)
{
  if (!t)
    return 0;
  int a = height (t->l), b = height (t->r);
  return 1 + (a > b ? a : b);
}

static OPT_NOINLINE int
contains (const struct tnode *t, uint32_t key)
{
  while (t && t->key != key)
    t = key < t->key ? t->l : t->r;
  return t != 0;
}

int
main (void)
{
  uint32_t s = opt_seed;
  struct tnode *root = 0;
  for (int i = 0; i < N; i++)
    {
      pool[i].key = opt_rand (&s) % 50000;
      insert (&root, &pool[i]);
    }
  uint32_t h = walk (root, 0);
  h = opt_mix (h, (uint32_t) height (root));
  for (int i = 0; i < N; i++)
    h = opt_mix (h, (uint32_t) contains (root, opt_rand (&s) % 50000));
  OPT_RESULT (h, 0xf09d81d9u);
}

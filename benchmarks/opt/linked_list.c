/* Singly linked list: sorted insertion, traversal, deletion of
   matching nodes.  Pointer chasing and null tests on every step.  */
#include "common.h"

#define N 300

struct node { struct node *next; uint32_t key; uint32_t val; };

static struct node pool[N];

static OPT_NOINLINE struct node *
insert_sorted (struct node *head, struct node *n)
{
  struct node **pp = &head;
  while (*pp && (*pp)->key < n->key)
    pp = &(*pp)->next;
  n->next = *pp;
  *pp = n;
  return head;
}

static OPT_NOINLINE struct node *
remove_if_odd (struct node *head)
{
  struct node **pp = &head;
  while (*pp)
    if ((*pp)->val & 1)
      *pp = (*pp)->next;
    else
      pp = &(*pp)->next;
  return head;
}

static OPT_NOINLINE uint32_t
sum_list (const struct node *n)
{
  uint32_t h = 0;
  for (; n; n = n->next)
    h = opt_mix (h, n->key + n->val);
  return h;
}

int
main (void)
{
  uint32_t s = opt_seed;
  struct node *head = 0;
  for (int i = 0; i < N; i++)
    {
      pool[i].key = opt_rand (&s) % 10000;
      pool[i].val = opt_rand (&s);
      head = insert_sorted (head, &pool[i]);
    }
  uint32_t h = sum_list (head);
  head = remove_if_odd (head);
  h = opt_mix (h, sum_list (head));
  OPT_RESULT (h, 0x84a48849u);
}

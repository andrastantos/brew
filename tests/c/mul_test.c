#include <stdio.h>

static void put_digit(int a, FILE *f)
{
  putc(a > 9 ? 'a'+a-10 : '0'+a, f);
}

static void put_hex(int a, FILE *f)
{
  put_digit((a >> 28) & 0xf, f);
  put_digit((a >> 24) & 0xf, f);
  put_digit((a >> 20) & 0xf, f);
  put_digit((a >> 16) & 0xf, f);
  put_digit((a >> 12) & 0xf, f);
  put_digit((a >>  8) & 0xf, f);
  put_digit((a >>  4) & 0xf, f);
  put_digit((a >>  0) & 0xf, f);
}

int main(void)
{
    int a,b,x;
    unsigned int c,d,y;

    printf("START\n");
    a = 0xffff;
    b = 0xffff;
    c = 0xffff;
    d = 0xffff;

    x = a*b;
    y = c*d;
    printf("BEFORE\n");
    putc('c', stdout);
    putc('b', stdout);
    putc('\n', stdout);
    put_hex(0xdeadbeef, stdout);
    putc('\n', stdout);
    
    printf("string: %s\n", "abc");
    printf("c: %i\n",c);
    printf("unsigned: %u * %u = %u\n",c,d,y);
    printf("signed:   %d * %d = %d\n",a,b,c);
    printf("AFTER\n");
    return 0;
}

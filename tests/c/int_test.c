#include <stdio.h>

int main(void)
{
  int a = 0xdeadbeef;
  int b = 33;
  unsigned int c = 0xdeadbeef;

  long long d = 0xdeadbeefdeadbeefLL;
  printf("%x %% %x = %x\n",a,b,a%b);
  printf("%x %% %x = %x\n",c,b,c%b);
  printf("%x / %x = %x\n",a,b,a/b);
  printf("%x / %x = %x\n",c,b,c/b);
  printf("%llx %% %x = %llx\n",d,b,d%b);
  return 0;
}

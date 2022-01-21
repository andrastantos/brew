//#include <stdio.h>

int main(void)
{
    int a,b,x;
    unsigned int c,d,y;

    a = 0xffff;
    b = 0xffff;
    c = 0xffff;
    d = 0xffff;

    x = a*b;
    y = c*d;
    printf("unsigned: %u * %u = %u\n",c,d,y);
    printf("signed:   %d * %d = %d\n",a,b,c);
    return 0;
}

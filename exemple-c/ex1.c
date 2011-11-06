#include <stdlib.h>
#include <stdio.h>

int
mult2 (int c)
{
  return c * 2;
}

int
main(void)
{
  printf("%d\n", mult2(3));
  int (*fmult2) (int) = mult2;
  printf("%d\n", (*fmult2)(3));
}

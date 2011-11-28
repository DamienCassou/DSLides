#include <stdlib.h>
#include <stdio.h>

#define MAX(a,b) ((a)>(b)?(a):(b))

int
main(void)
{
  printf("max(1,5)=%d\n", MAX(1,5));
  printf("max(10,1)=%d\n", MAX(10,1));
}

#include <stdlib.h>
#include <stdio.h>

int
f (int c)
{
  printf("f\n");
  return c;
}

int
g (int c)
{
  printf("g\n");
  return c;
}

int
h (int c)
{
  printf("h\n");
  return c;
}

int
main(void)
{
  f(g(h(1)));
}

#include <stdio.h>

extern int foobar(float x);

int main() {
  printf("%d\n", foobar(1337.0));
  return 0;
}

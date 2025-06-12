#include <stddef.h>

typedef void* VOID_PTR;

void
foo(int a __attribute__((unused)),
    char b __attribute__((unused)),
    VOID_PTR c __attribute__((unused)));

int
main()
{
  foo(0, 0, NULL);
  return 0;
}

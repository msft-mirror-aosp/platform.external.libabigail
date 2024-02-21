// To compile this type:
//   g++ -g -fPIC -Wall -shared -o libtest6-undefined-var.so test6-undefined-var.cc

#include "test6-var-changed-libapp-v0.h"

int
abracadabra()
{
  S0* s0 = bar;
  S1* s1 = foo;
  return s0->m0 + s1->m0;
}

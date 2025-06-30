/* gcc -g -shared -o impl1.so bug1.c */

#include "interface.h"
int public_func1(int a) {
  return a;
}
double public_func2(double a) {
  return a;
}
float _private_func1(float a) {
  return a;
}

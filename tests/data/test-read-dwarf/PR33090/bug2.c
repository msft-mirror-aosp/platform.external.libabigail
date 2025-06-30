/* gcc -g -shared -o impl2.so bug2.c */

#include "interface.h"
int public_func1(int a) {
  return a;
}
double public_func2(double a) {
  return a;
}
float _private_func2(float a) {
  return a;
}

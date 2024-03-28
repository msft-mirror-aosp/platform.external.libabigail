// g++ -shared -g -o libtest4-v0.so test4-v0.cc

struct type
{
  int m0;
  char m1;
};

int
foo(type& t)
{
  return t.m1;
}

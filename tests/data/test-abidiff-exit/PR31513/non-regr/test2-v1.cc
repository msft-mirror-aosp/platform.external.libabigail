// g++ -shared -g -o libtest2-v1.so test2-v1.cc
struct base
{
  int m0;
  char m1;
  char m2;
};

struct type : public base
{
};

int
foo(type& t)
{
  return t.m1;
}

// g++ -shared -g -o libtest1-v1.so test1-v1.cc
struct base
{
  int m0;
  char m1;
};

struct type : public base
{
};

int
foo(type& t)
{
  return t.m1;
}

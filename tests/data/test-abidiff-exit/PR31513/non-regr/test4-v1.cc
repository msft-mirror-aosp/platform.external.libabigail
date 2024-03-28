// g++ -shared -g -o libtest4-v1.so test4-v1.cc
struct base
{
  int m0;
  float m2;
};

struct type : public base
{
  char m3;
};

int
foo(type& t)
{
  return t.m0;
}

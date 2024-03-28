// g++ -shared -g -o libtest3-v1.so test3-v1.cc
struct base
{
  int m0;
  char m1;
  float m2;
};

struct type : public base
{
};

int
foo(type& t)
{
  return t.m1;
}

struct X {
  void f(int);
  int a;
};
struct Y;

int X::* pmi = &X::a;
void (X::* pmf)(int) = &X::f;
double X::* pmd;
char Y::* pmc;

typedef struct { int t; } T;
auto pmy = &T::t;

namespace {
struct Z { int z; };
}
int Z::* pmz = &Z::z;

union U { int u; };
auto pmu = &U::u;

typedef const U CU;
auto pmcu = &CU::u;

int
main()
{
  return 0;
}

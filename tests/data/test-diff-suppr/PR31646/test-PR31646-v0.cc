struct opaque
{
  int m0;
  int m1;
};

struct type
{
  int m0;
  opaque* m1;
  int m2;
};

opaque*
fun(type&)
{
}

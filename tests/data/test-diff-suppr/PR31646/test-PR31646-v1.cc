struct opaque
{
  int m0;
  char m_inserted;
  int m1;
};

struct type
{
  int m0;
  opaque* m1;
  int m2;
};

void
fun(type&)
{
}

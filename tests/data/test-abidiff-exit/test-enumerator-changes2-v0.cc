enum E
{
  FIRST_E,
  SECOND_E = 1,
  THIRD_E = 2,
  FOURTH_E = 3,
  FIFTH_E = 4
};

enum E
foo()
{
  enum E e = FIRST_E;
  return e;
}

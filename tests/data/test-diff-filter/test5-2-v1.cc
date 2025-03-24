// g++ -g -Wall -c test5-2-v1.cc

struct C0
{
  int m0;

  C0()
    :m0(0)
  {}

  virtual int vfn1() {return 0;}
  virtual int vfn2(char) {return 0;}
};

typedef C0 c0_type;

c0_type
foo()
{return C0();}

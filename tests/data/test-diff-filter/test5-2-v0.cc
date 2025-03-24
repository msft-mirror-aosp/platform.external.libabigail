// g++ -g -Wall -c test5-2-v0.cc

class C0
{
  int m0;

public:
  C0()
    :m0(0)
  {}

  virtual int vfn1() {return 0;}
  virtual unsigned vfn2(char) {return 0;}
};

C0
foo()
{return C0();}

struct type
{
  struct /* wrapping up all data members into an anonymous data
	    member */
  {
    int m0;
    struct
    {
      union {int um00; char* um01;};
      int* m1;
      union {int* um10; char um11;};
    };
    float m2;
  };
};

void
foo(struct type* __attribute__((unused)))
{
}

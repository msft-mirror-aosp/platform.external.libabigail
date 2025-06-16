class Interface
{
public:
  virtual ~Interface() {};

  virtual Interface* clone() = 0;

#ifdef NEW_VERSION
  virtual void someNewMethod() = 0;
#endif
};

class PublicImplementation
{
public:
  PublicImplementation();

  virtual ~PublicImplementation();

#ifdef NEW_VERSION
  void someNewMethod();
#endif

private:
  Interface* _pImpl;
};

PublicImplementation::PublicImplementation() = default;
PublicImplementation::~PublicImplementation() = default;

#ifdef NEW_VERSION
void PublicImplementation::someNewMethod() {}
#endif

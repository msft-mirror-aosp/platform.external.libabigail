struct foo {
  long z;
};

struct ops1 {
  int ** x;
};

struct ops2 {
  foo y[10];
};

struct ops3 {
  void (*spong)(int && wibble);
};

void register_ops1(ops1*) {
}

void register_ops2(ops2*) {
}

void register_ops3(ops3*) {
}

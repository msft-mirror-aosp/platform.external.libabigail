// Compile with:
//
// g++ -g -L. -ltest10-with-exported-symbols -o  test10-app-with-undefined-symbols test10-app-with-undefined-symbols.cc
//

#include "test10-with-exported-symbols.h"

int
main()
{
  some_type s;
  return s.get_first_member() + s.get_second_member();
}

// Build this with:
// g++ -g -Wall -shared  -o libtest10-with-exported-symbols.so test10-with-exported-symbols.cc

#include "test10-with-exported-symbols.h"

int
some_type::get_first_member()
{return first_member;}

void
some_type::set_first_member(int v)
{first_member = v;}

char
some_type::get_second_member()
{return second_member;}

void
some_type::set_second_member(char v)
{second_member = v;}

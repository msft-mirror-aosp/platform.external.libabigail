/*
 * Compile this with:
 *   gcc -g -c test-has-strict-flexible-array-data-member-conversion-v1.c
 */
struct foo
{
  int x;
  int flex[];
};

struct foo S;

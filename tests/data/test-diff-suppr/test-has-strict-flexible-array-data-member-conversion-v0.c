/*
 * Compile this with:
 *   gcc -g -c test-has-strict-flexible-array-data-member-conversion-v0.c
 */
struct foo
{
  int x;
  int flex[1];
};

struct foo S;

#include <stdio.h>

char *backcall( char *arg){
  return arg+1;
}

char *libcall( char *arg);

int main(int argc, char **argv){
  printf("%s\n", libcall(argv[0]));
  return 0;
}

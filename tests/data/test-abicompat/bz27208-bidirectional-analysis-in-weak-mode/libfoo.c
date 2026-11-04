#include "backcall-v1.h"

char *libcall( char *arg){
  return (char*)backcall((unsigned*)arg);
}

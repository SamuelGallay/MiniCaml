#include "stdio.h"

__int8_t* print_int(__int8_t* x){
  int *i = (int *) x;
  printf("%i ", *i);
  return NULL;
}


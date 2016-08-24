#include <unistd.h>
void sleep_(sec)
int *sec;

 { 
  sleep (*sec);
  return;
 }

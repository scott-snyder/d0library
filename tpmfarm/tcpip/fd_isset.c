/* Fortran interface to a system macro FD_SET */
#define IBMAIX 2
#define _XOPEN_SOURCE
#include <sys/select.h>

int fd_isset_(descr, descr_set)
int *descr;
int *descr_set;
{
int bit;
fd_set desbits;
  desbits.fds_bits[0] = *descr_set;
  bit = FD_ISSET(*descr, &desbits);
  return ( bit );
}

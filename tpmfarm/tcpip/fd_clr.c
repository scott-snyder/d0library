/* Fortran interface to a system macro FD_SET */
#define IBMAIX 2
#define _XOPEN_SOURCE
#include <sys/select.h>

void fd_clr_(descr, descr_set)
int *descr;
int *descr_set;
{
fd_set desbits;
  desbits.fds_bits[0] = *descr_set;
  FD_CLR(*descr, &desbits);
  *descr_set = desbits.fds_bits[0];
}

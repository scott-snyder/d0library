/* Fortran interface to a system macro FD_SET */
#define IBMAIX 2
#if D0FLAVOR == IBMAIX
        #define HDR <sys/select.h>
#else
        #define HDR <sys/types.h>
#endif

#include HDR

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

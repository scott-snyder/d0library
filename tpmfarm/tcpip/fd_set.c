/* Fortran interface to a system macro FD_SET */
#define IBMAIX 2
#if D0FLAVOR == IBMAIX
        #define HDR <sys/select.h>
#else
        #define HDR <sys/types.h>
#endif

#include HDR

void fd_set_(descr, descr_set)
int *descr;
int *descr_set;
{
fd_set desbits;
  desbits.fds_bits[0] = *descr_set;
  FD_SET(*descr, &desbits);
  *descr_set = desbits.fds_bits[0];
}

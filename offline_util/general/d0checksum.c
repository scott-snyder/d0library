/*
 *-
 *-   Purpose and Methods: Fortran callable interface to compute a 32 bit
 *-       checksum.
 *-
 *-   Inputs  : array   - Array containing data to be checksummed
 *-             n       - length of array
 *-             initsum - Initial value of checksum (allow rolling updates)
 *-   Outputs : thesum  - Checksum
 *-   Controls:
 *-
 *-   Created   4-Jan-1995   John D. Hobbs
 *-
*/
/* VMS/UNIX compatibility. */
 
#ifdef vms
#define FNAME(a) a
#else
#define FNAME(a) a##_
#endif

#define ULONG_SIZE  4
#define MAXBITS 32

#define ulong unsigned long
ulong d0checksum( ulong *, ulong, ulong, ulong);

ulong FNAME(checksum32)(ulong *array, ulong *n, ulong *initsum)
{
  return(d0checksum(array,*n,*initsum,32));
}

/*
 *-
 *-   Purpose and Methods: Compute an nbit bit checksum.  If the 
 *-      size of an unsigned long is not ULONG_BITS, return 0.  The data in the
 *-      input array are treated as a sequence of nbit quantities not as
 *-      unsigned longs...
 *-
 *-   Inputs  : array   - Bitstream (integer*32)  to be checksummed
 *-             n       - number of nbits elements.
 *-             initsum - the initial value of the checksum
 *-             nbits   - the number of bits used in the checksum.  Must
 *-               be an integer divisor of MAXBITS (ie, mod(MAXBITS,nbits)=0)
 *-   Outputs :
 *-   Controls:
 *-
 *-   Created   4-Jan-1995   John D. Hobbs
 *-
*/
ulong d0checksum(ulong *array, ulong n, ulong initsum, ulong nbits)
{
  ulong thesum,maxnum,i,delta,curword,curpos,datum,NPerWord;

  /* Check for expected sizes. */
  if( sizeof(ulong) != ULONG_SIZE ) return(0);
  if( nbits>MAXBITS || nbits==0 ) return(0);
  if( (MAXBITS%nbits)!=0 ) return(0);
  NPerWord = MAXBITS/nbits;

  /* Setup the bit size of the checksum. */
  maxnum=0;
  for( i=0 ; i<nbits ; i++ ) maxnum |= (1<<i);

  /* Compute the checksum. */
  thesum=initsum;
  for( i=0 ; i<n ; i++ ) {

    /* Extract the next nbit sequence. */
    curword=i/NPerWord;
    curpos=i%NPerWord;
    datum = (array[curword]>>(curpos*nbits)) & maxnum;

    /* Compute the checksum. */
    delta=maxnum-thesum;
    if( datum>delta ) thesum = datum-delta-1;
    else thesum += datum;
  }
  return(thesum&maxnum);
}

 

/*
 *-
 *-   Purpose and Methods: Dummy uncompression module for UNCOMPRESS_ZEBRA
 *-                        pbd package.
 *-
 *-   Fortran-callable entry points:
 *-
 *-   void uncompress_none(void)
 *-
 *-       Main uncompression routine.  Copies plain text from input to 
 *-       output stream until end-of-data.
 *-
 *-   Created  17-Oct-1994   Herbert Greenlee
 *-
*/
/* VMS/UNIX compatibility. */

#ifdef vms
#define FNAME(a) a
#else
#define FNAME(a) a##_
#endif

/*
 * This is a dummy uncompression module for the UNCOMPRESS_ZEBRA package.
 * It simply copies plaintext from the input stream to the output stream.
 * It is intended for excersizing and debugging the other parts of the
 * package.
 */

/* Define input and output macros for bits and bytes. */

#define Putc(a,b)            (arg1=(a),\
                             FNAME(uncompress_zebra_event_putc)(&arg1))
#define Flushc()             (FNAME(uncompress_zebra_event_flushc)())
#define InputBits(a,b)       (arg1=(b),\
                              FNAME(uncompress_zebra_event_getb)(&arg1))

/* Space for literal arguments to fortran routines. */

static int arg1;

/* Function prototypes for fortran-callable c routines. */

void FNAME(uncompress_none)(void);

/* Function prototypes for fortran routines called from c. */

int FNAME(uncompress_zebra_event_getb)(int *nbits);
int FNAME(uncompress_zebra_event_putc)(int *data);
int FNAME(uncompress_zebra_event_flushc)();

/*
 * Trivial uncompressor routine. 
 */

void FNAME(uncompress_none)(void)
{
    int character;

    while( ( character = InputBits( input, 8 ) ) >= 0)
        Putc( character, output );
    Flushc();
}

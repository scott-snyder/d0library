/*
 *-
 *-   Purpose and Methods: Dummy compression module for COMPRESS_ZEBRA
 *-                        pbd package.
 *-
 *-   Fortran-callable entry points:
 *-
 *-   void compress_none_ini(void)
 *-
 *-       Dummy event initialization routine.  Does nothing.
 *-
 *-   void compress_none(void)
 *-
 *-       Main compression routine.  Copies plain text from input to 
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
 * This is a dummy compression module for the COMPRESS_ZEBRA package.
 * It simply copies plaintext from the input stream to the output stream.
 * It is intended for excersizing and debugging the other parts of the
 * package.
 */
	
/* Define input and output macros for bits and bytes. */

#define Getc(a)              (FNAME(compress_zebra_event_getc)())
#define OutputBits(a,b,c)    (arg1=(b),arg2=(c),\
                              FNAME(compress_zebra_event_putb)(&arg1,&arg2))
#define FlushBits()          (FNAME(compress_zebra_event_flushb)())

/* Space for literal arguments to fortran routines. */

static int arg1, arg2;       

/* Function prototypes for fortran-callable c routines. */

void FNAME(compress_none_ini)(void);
void FNAME(compress_none)(void);

/* Fortran routines called from c. */

int FNAME(compress_zebra_event_getc)(void);
int FNAME(compress_zebra_event_putb)(int *data, int *nbits);
int FNAME(compress_zebra_event_flushb)();

/*
 * Dummy initialization routine. 
*/
void FNAME(compress_none_ini)(void)
{
    return;
}

/*
 * Trivial compressor routine. 
 */

void FNAME(compress_none)(void)
{
    int character;

    while ( ( character = Getc( input ) ) >= 0 )
        OutputBits( output, character, 8 );
    FlushBits();
}

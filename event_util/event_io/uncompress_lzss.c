/*
 *-
 *-   Purpose and Methods: LZSS uncompression module for UNCOMPRESS_ZEBRA
 *-                        pbd package.
 *-
 *-   Fortran-callable entry points:
 *-
 *-   void uncompress_lzss(void)
 *-
 *-       Main uncompression routine.
 *-
 *-   Created  17-Oct-1994   Herbert Greenlee
 *-
 *-   The code in this file is adapted from freeware source code included
 *-   with the book "The Data Compression Book" by Mark Nelson, M & T Books,
 *-   San Mateo, CA (1992).
 *-
*/
/* VMS/UNIX compatibility. */

#ifdef vms
#define FNAME(a) a
#else
#define FNAME(a) a##_
#endif

/************************** Start of LZSS.C *************************
 *
 * This is the LZSS module, which implements an LZ77 style compression
 * algorithm.  
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/*
 * Various constants used to define the compression parameters.  
 * END_OF_STREAM is a special index used to flag the fact that the file 
 * has been completely encoded, and there is no more data.  MOD_WINDOW() 
 * is a macro used to perform arithmetic on window indices.
 */

#define END_OF_STREAM        0
#define UNUSED               0
#define MOD_WINDOW( a )      ( ( a ) & ( window_size - 1 ) )

/* Define input and output macros for bits and bytes. */

#define Putc(a,b)            (arg1=(a),\
                              FNAME(uncompress_zebra_event_putc)(&arg1))
#define Flushc()             (FNAME(uncompress_zebra_event_flushc)())
#define InputBit(a)          (arg1=1,\
                              FNAME(uncompress_zebra_event_getb)(&arg1))
#define InputBits(a,b)       (arg1=(b),\
                              FNAME(uncompress_zebra_event_getb)(&arg1))

/* Space for literal arguments to fortran routines. */

static int arg1;

/*
 * The window[] array is exactly that, the window of previously seen
 * text, as well as the current look ahead text.  
*/

static unsigned char *window = NULL;

/*
 * Other global varables.  Index_bit_count tells how many bits we allocate 
 * to indices into the text window.  This directly determines window_size.  
 * Length_bit_count tells how many bits we allocate for the length of
 * an encode phrase.  Break_even is the minimum number of characters in a 
 * phrase.  It represents an offset to the encoded length.  
 */

static unsigned int index_bit_count;
static unsigned int length_bit_count;
static unsigned int window_size = 0;
static unsigned int break_even;

/* Function prototypes for fortran-callable c routines. */

void FNAME(uncompress_lzss)(void);

/* Function prototypes for fortran routines called from c. */

int FNAME(uncompress_zebra_event_getb)(int *nbits);
int FNAME(uncompress_zebra_event_putc)(int *data);
int FNAME(uncompress_zebra_event_flushc)();

/* Local prototypes. */

static void InitializeWindow();

/*
 * This routine allocates the window buffer.  It initially allocates space 
 * for the window or expands it to the size required by index_length_count.
 * It does nothing if the window is already allocated with enough space.
 */

static void InitializeWindow()
{
    void *new_window;
    unsigned int new_window_size;

/* Calculate minimum required window size. */

    new_window_size = 1 << index_bit_count;

/* Make sure that the window is allocated and is big enough. */

    if ( window_size < new_window_size || window == NULL ) {
        new_window = malloc( new_window_size );
	if ( new_window == NULL ) {
	    printf("Uncompess_lzss: malloc failed in InitializeWindow\n");
	    abort();
	}
	if( window != NULL ) {
	    memcpy( new_window, window, window_size);
	    free( window );
	}
	window = new_window;
	window_size = new_window_size;
    }
}

/*
 * This is the expansion routine for the LZSS algorithm.  All it has
 * to do is read in flag bits, decide whether to read in a character or
 * a index/length pair, and take the appropriate action.
*/

void FNAME(uncompress_lzss)( void )
{
    int i;
    int current_position;
    int c;
    int match_length;
    int match_position;

/* Re-establish the token sizes and current position stored by the 
   compression routine. */

    index_bit_count = (unsigned int) InputBits( input, 5 );
    length_bit_count = (unsigned int) InputBits( input, 5 );
    break_even = (unsigned int) InputBits( input, 4 );
    InitializeWindow();
    current_position = (int) InputBits( input, index_bit_count );

/* Main loop. */

    for ( ; ; ) {
        if ( InputBit( input ) ) {
            c = (int) InputBits( input, 8 );
            Putc( c, output );
            window[ current_position ] = (unsigned char) c;
            current_position = MOD_WINDOW( current_position + 1 );
        } else {
            match_position = (int) InputBits( input, index_bit_count );
            if ( match_position == END_OF_STREAM )
                break;
            match_length = (int) InputBits( input, length_bit_count );
            match_length += break_even;
            for ( i = 0 ; i <= match_length ; i++ ) {
                c = window[ MOD_WINDOW( match_position + i ) ];
                Putc( c, output );
                window[ current_position ] = (unsigned char) c;
                current_position = MOD_WINDOW( current_position + 1 );
            }
        }
    }

/* Flush output buffer. */

    Flushc();
}

/************************** End of LZSS.C *************************/

/*
 *-
 *-   Purpose and Methods: LZW uncompression module for UNCOMPRESS_ZEBRA
 *-                        pbd package.
 *-
 *-   Fortran-callable entry points:
 *-
 *-   void uncompress_lzw(void)
 *-
 *-       Main compression routine.
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

/************************** Start of LZW15V.C *************************
 *
 * This is the LZW module which implements a more powerful version
 * of the algorithm.  Unlike the compressor, the uncompressor implemented
 * here has no intrinsic limit to the size of the dictionary.  There are
 * two indefinite size data structures (the dictionary and the decode stack)
 * that grow in size indefinitely.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * Constants used throughout the program.  The code defines should be
 * self-explanatory.  
 */

#define END_OF_STREAM              256
#define BUMP_CODE                  257
#define FLUSH_CODE                 258
#define FIRST_CODE                 259
#define UNUSED                     -1

/* Define input and output macros for bits and bytes. */

#define Putc(a,b)            (arg1=(a),\
                              FNAME(uncompress_zebra_event_putc)(&arg1))
#define Flushc()             (FNAME(uncompress_zebra_event_flushc)())
#define InputBits(a,b)       (arg1=(b),\
                              FNAME(uncompress_zebra_event_getb)(&arg1))

/* Space for literal arguments to fortran routines. */

static int arg1;

/* Function prototypes for local routines. */

static unsigned int decode_string( unsigned int offset, unsigned int code );
static void InitializeStorage();
static void InitializeDictionary();

/* Function prototypes for fortran-callable c routines. */

void FNAME(uncompress_lzw)(void);

/* Function prototypes for fortran routines called from c. */

int FNAME(uncompress_zebra_event_getb)(int *nbits);
int FNAME(uncompress_zebra_event_putc)(int *data);
int FNAME(uncompress_zebra_event_flushc)(void);

/*
 * This data structure defines the dictionary.  Each entry in the dictionary
 * has a code value.  This is the code emitted by the compressor.  Each
 * code is actually made up of two pieces:  a parent_code, and a
 * character.  Code values of less than 256 are actually plain
 * text codes.
 */

static unsigned int table_size = 0;
static struct dictionary {
    int parent_code;
    char character;
} *dict = NULL;

/*
 * Other global data structures.  The decode_stack is used to reverse
 * strings that come out of the tree during decoding.  next_code is the
 * next code to be added to the dictionary, both during compression and
 * decompression.  current_code_bits defines how many bits are currently
 * being used for output, and next_bump_code defines the code that will
 * trigger the next jump in word size.
 */

static char *decode_stack = NULL;
static unsigned int decode_stack_size = 0;
static unsigned int next_code;
static int current_code_bits;

/*
 * This routine is used to initialize the dictionary, both when the
 * compressor or decompressor first starts up, and also when a flush
 * code comes in.  
 */

static void InitializeDictionary()
{
    current_code_bits = 9;
    next_code = FIRST_CODE;
    InitializeStorage();
}

/*
 * This routine allocates the dictionary.  It initially allocates space for
 * the dictionary or expands it to the size required by current_code_bits.
 * It does nothing if the dictionary is already allocated with enough space.
 */

static void InitializeStorage()
{
    void *new_dict;
    unsigned int new_table_size;

/* Calculate minimum required dictionary size. */

    new_table_size = 1 << current_code_bits;

/* Make sure that the dictionary is allocated and is big enough. */

    if ( table_size < new_table_size || dict == NULL ) {
        new_dict = malloc( new_table_size*sizeof(*dict) );
	if ( new_dict == NULL ) {
	    printf("Uncompess_lzw: malloc failed in InitializeStorage\n");
	    abort();
	}
	if( dict != NULL ) {
	    memcpy( new_dict, dict, table_size*sizeof(*dict));
	    free( dict );
	}
	dict = new_dict;
	table_size = new_table_size;
    }
}

/*
 * The file expander operates much like the encoder.  It has to
 * read in codes, the convert the codes to a string of characters.
 * The only catch in the whole operation occurs when the encoder
 * encounters a CHAR+STRING+CHAR+STRING+CHAR sequence.  When this
 * occurs, the encoder outputs a code that is not presently defined
 * in the table.  This is handled as an exception.  All of the special
 * input codes are handled in various ways.
 */

void FNAME(uncompress_lzw)(void)
{
    unsigned int new_code;
    unsigned int old_code;
    int character;
    unsigned int count;

/* Read in the bit-size of tokens. */

    current_code_bits = InputBits( input, 6 );

/* Initialize old_code to null string at the beginning of an event and
   after a dictionary init. */

    old_code = 2 * table_size;

/* Main token reading loop. */

    for ( ; ; ) {
        new_code = (unsigned int) InputBits( input, current_code_bits );

  /* End of data. */

        if ( new_code == END_OF_STREAM ) {
            Flushc();
            return;
	}

  /* Flush token -> (Re-)Initialize dictionary.  This should always be the
     first token in a new file and for each event in random mode. */

        if ( new_code == FLUSH_CODE ) {
            InitializeDictionary();
	    old_code = 2 * table_size;
	    continue;
	}

  /* Bump token -> Increase token size by one bit.  Increase dictionary if
     necessary. */

	if ( new_code == BUMP_CODE ) {
	    current_code_bits++;
	    InitializeStorage();
	    continue;
        }

  /* String token.  Check for undefined string exception.  Then decode
     token to the actual string. */

	if ( new_code >= next_code ) {
	    if(old_code >= table_size ) {
	        printf("Uncompress_lzw: Initial code not in dictionary\n");
		abort();
	    }
	    count = decode_string( 1, old_code );
	    decode_stack[ 0 ] = (char) character;
        }
	else
            count = decode_string( 0, new_code );

  /* Output decoded string and update. */

	character = decode_stack[ count - 1 ];
	while ( count > 0 )
            Putc( decode_stack[ --count ], output );

  /* Update dictionary. */

	if ( old_code < table_size ) {
	    dict[ next_code ].parent_code = old_code;
	    dict[ next_code ].character = (char) character;
	    next_code++;
	}
	old_code = new_code;
    }
}

/*
 * This routine decodes a string from the dictionary, and stores it
 * in the decode_stack data structure.  It returns a count to the
 * calling program of how many characters were placed in the stack.
 */

static unsigned int decode_string( unsigned int count, unsigned int code )
{

    char *new_decode_stack;
    unsigned int new_decode_stack_size;

    for(;;) {

/* Make sure that the decode stack is long enough and increase if necessary. */

        if ( decode_stack_size < count+1 || decode_stack == NULL ) {
	    new_decode_stack_size = (9*(count+1))/8;
	    new_decode_stack = malloc( new_decode_stack_size );
	    if ( new_decode_stack == NULL ) {
	        printf("Uncompess_lzw: malloc failed in decode_string\n");
		abort();
	    }
	    if( decode_stack != NULL ) {
	        memcpy( new_decode_stack, decode_stack, decode_stack_size);
	        free( decode_stack );
	    }
	    decode_stack = new_decode_stack;
	    decode_stack_size = new_decode_stack_size;
	}

/* Add next (last) character to decode_stack. */

	if ( code < 256 ) {
	    decode_stack[ count++ ] = (char) code;
	    return( count );
	}
	else {
	    decode_stack[ count++ ] = dict[ code ].character;
	    code = dict[ code ].parent_code;
	}
    }
}


/************************** End of LZW15V.C *************************/

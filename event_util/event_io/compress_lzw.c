/*
 *-
 *-   Purpose and Methods: LZW compression module for COMPRESS_ZEBRA 
 *-                        pbd package.
 *-
 *-   Fortran-callable entry points:
 *-
 *-   void compress_lzw_param(int *dict_bits, int *length_bits)
 *-
 *-       This routine must be called once to perform overal initialization
 *-       and set the maximum number of bits in the dictionary address, which
 *-       determines the maximum size of the dictionary.
 *-
 *-   void compress_lzw_ini(void)
 *-
 *-       This routine may be called to reinitialize the dictionary between
 *-       events.  It need not be called at all in sequential mode.
 *-
 *-   void compress_lzw(void)
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
 * of the algorithm.  This version of the program has three major
 * improvements over LZW12.C.  First, it expands the maximum code size
 * to 15 bits.  Second, it starts encoding with 9 bit codes, working
 * its way up in bit size only as necessary.  Finally, it flushes the
 * dictionary when done.
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * Constants used throughout the program.  The code defines 
 * should be self-explanatory.  
 */

#define END_OF_STREAM              256
#define BUMP_CODE                  257
#define FLUSH_CODE                 258
#define FIRST_CODE                 259
#define UNUSED                     -1

/* Define input and output macros for bits and bytes. */

#define Getc(a)              (FNAME(compress_zebra_event_getc)())
#define OutputBits(a,b,c)    (arg1=(b),arg2=(c),\
                              FNAME(compress_zebra_event_putb)(&arg1,&arg2))
#define FlushBits()          (FNAME(compress_zebra_event_flushb)())

/* Space for literal arguments to fortran routines. */

static int arg1, arg2;       

/* Function prototypes for local routines. */

static int prime( unsigned int n );
static unsigned int find_child_node( int parent_code, int child_character );
static void InitializeDictionary();

/* Function prototypes for fortran callable c routines. */

void FNAME(compress_lzw_param)( int *dict_bits );
void FNAME(compress_lzw_ini)(void);
void FNAME(compress_lzw)(void);

/* Function prorotypes for fortran routines called from c. */

int FNAME(compress_zebra_event_getc)(void);
int FNAME(compress_zebra_event_putb)(int *data, int *nbits);
int FNAME(compress_zebra_event_flushb)(void);

/*
 * This data structure defines the dictionary.  Each entry in the dictionary
 * has a code value.  This is the code emitted by the compressor.  Each
 * code is actually made up of two pieces:  a parent_code, and a
 * character.  Code values of less than 256 are actually plain
 * text codes.
 */

static struct dictionary {
    int code_value;
    int parent_code;
    char character;
} *dict = NULL;

/*
 * Other global data structures.  
 * next_code is the  next code to be added to the dictionary, both during 
 * compression and decompression.
 * current_code_bits defines how many bits are currently being used for output.
 * next_bump_code defines the code that will trigger the next jump in word 
 * size.  
 * table_size is set during initialization to a prime number that is large
 * enough to hold the dictionary table with about 20% to spare.
 */

static unsigned int bits = 0;
static unsigned int max_code = 0;
static unsigned int table_size = 0;
static unsigned int next_code;
static int current_code_bits;
static unsigned int next_bump_code;

/*
 * This routine is used to initialize the dictionary, both when the
 * compressor or decompressor first starts up, and also when a flush
 * code comes in.  
 */

static void InitializeDictionary()
{
    unsigned int i;

    for ( i = 0 ; i < table_size ; i++ )
        dict[ i ].code_value = UNUSED;
    next_code = FIRST_CODE;
    current_code_bits = 9;
    next_bump_code = 511;
}

/* 
 * Routine to determine whether or not an integer is prime.  Used to determine
 * the size of the hash table during initialization.
*/

static int prime( unsigned int n )
{
  unsigned int i, j;
  
  if( n == 2 || n == 3 )
    return 1;
  if( n < 5 )
    return 0;
  if( n%2 == 0 )
    return 0;
  for(i=3 ;; i+=2) {
    j = n/i;
    if( i*j == n )
      return 0;
    if( j < i )
      return 1;
  }
}
    

/*
 * This fortran-callable entry point is used to set compression parameters
 * and allocate space for the dictionary.
*/
void FNAME(compress_lzw_param)( int *dict_bits )
{
    bits = *dict_bits;
    max_code = (1 << bits) - 1;

/* Calculate table_size to be about 20% larger than the amount of
   space needed to hold the dictionary, and prime. */

    table_size = 1.2 * (float)max_code;
    while( ! prime(++table_size) );

/* Now allocate space for the dictionary. */

    if(dict != NULL)
        free(dict);
    dict = (struct dictionary *)
                 calloc( table_size, sizeof ( struct dictionary ) );
    if ( dict == NULL ) {
        printf("Compress_lzw: malloc failed\n");
        abort();
    }
    InitializeDictionary();
    return;
}
 
/*
 * This fortran-callable entry point re-initializes the dictionary to be empty.
*/
void FNAME(compress_lzw_ini)(void)
{
    InitializeDictionary();
    return;
}

/*
 * The compressor is short and simple.  It reads in new symbols one
 * at a time from the input file.  It then  checks to see if the
 * combination of the current symbol and the current code are already
 * defined in the dictionary.  If they are not, they are added to the
 * dictionary, and we start over with a new one symbol code.  If they
 * are, the code for the combination of the code and character becomes
 * our new code.  Note that in this enhanced version of LZW, the
 * encoder needs to check the codes for boundary conditions.
 */

void FNAME(compress_lzw)(void)
{
    int character;
    int string_code;
    unsigned int index;

/* Output current_code_bits for expander. */

    OutputBits( output, current_code_bits, 6 );

/* If we have just initialized the dictionary (next_code == FIRST_CODE), 
   then output the flush token. */

    if( next_code == FIRST_CODE ) {
        OutputBits( output, (unsigned long) FLUSH_CODE, current_code_bits );
    }

    if ( ( string_code = Getc( input ) ) < 0 )
        string_code = END_OF_STREAM;
    while ( ( character = Getc( input ) ) >= 0 ) {
        index = find_child_node( string_code, character );
        if ( dict[ index ].code_value != UNUSED )
            string_code = dict[ index ].code_value;
        else {
            dict[ index ].code_value = next_code++;
            dict[ index ].parent_code = string_code;
            dict[ index ].character = (char) character;
            OutputBits( output,
                        (unsigned long) string_code, current_code_bits );
            string_code = character;
            if ( next_code > max_code ) {
                OutputBits( output,
                            (unsigned long) FLUSH_CODE, current_code_bits );
                InitializeDictionary();
            } else if ( next_code > next_bump_code ) {
                OutputBits( output,
                            (unsigned long) BUMP_CODE, current_code_bits );
                current_code_bits++;
                next_bump_code <<= 1;
                next_bump_code |= 1;
            }
        }
    }
    OutputBits( output, (unsigned long) string_code, current_code_bits );
    OutputBits( output, (unsigned long) END_OF_STREAM, current_code_bits);

/* Flush output buffer. */

    FlushBits();
}

/*
 * This hashing routine is responsible for finding the table location
 * for a string/character combination.  The table index is created
 * by using an exclusive OR combination of the prefix and character.
 * This code also has to check for collisions, and handles them by
 * jumping around in the table.
 */

static unsigned int find_child_node( int parent_code, int child_character )
{
    unsigned int index;
    int offset;

    index = ( child_character << ( bits - 8 ) ) ^ parent_code;
    if ( index == 0 )
        offset = 1;
    else
        offset = table_size - index;
    for ( ; ; ) {
        if ( dict[ index ].code_value == UNUSED )
            return( (unsigned int) index );
        if ( dict[ index ].parent_code == parent_code &&
             dict[ index ].character == (char) child_character )
            return( index );
        if ( (int) index >= offset )
            index -= offset;
        else
            index += table_size - offset;
    }
}

/************************** End of LZW15V.C *************************/

/*
 *-
 *-   Purpose and Methods: LZSS compression module for COMPRESS_ZEBRA
 *-                        pbd package.
 *-
 *-   Fortran-callable entry points:
 *-
 *-   void compress_lzss_param(int *index_bits, int *length_bits)
 *-
 *-       This routine must be called once to perform overal initialization
 *-       and set the compression parameters.  It may be called multiple
 *-       times to reset the compression parameters.
 *-
 *-   void compress_lzss_ini(void)
 *-
 *-       This routine may be called to reinitialize the dictionary between
 *-       events.  It need not be called at all in sequential mode.
 *-
 *-   void compress_lzss(void)
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

/************************** Start of LZSS.C *************************
 *
 * This is the LZSS module, which implements an LZ77 style compression
 * algorithm.  
 * of between 2 and 17 bytes.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/*
 * The TREE_ROOT is a special node in the tree that always points to
 * the root node of the binary phrase tree.  END_OF_STREAM is a special
 * index used to flag the fact that the file has been completely
 * encoded, and there is no more data.  UNUSED is the null index for
 * the tree. MOD_WINDOW() is a macro used to perform arithmetic on tree
 * indices.
 *
 */

#define TREE_ROOT            window_size
#define END_OF_STREAM        0
#define UNUSED               0
#define MOD_WINDOW( a )      ( ( a ) & ( window_size - 1 ) )

/* Define input and output macros for bits and bytes. */

#define Getc(a)              (FNAME(compress_zebra_event_getc)())
#define OutputBit(a,b)       (arg1=(b),arg2=1,\
                              FNAME(compress_zebra_event_putb)(&arg1,&arg2))
#define OutputBits(a,b,c)    (arg1=(b),arg2=(c),\
                              FNAME(compress_zebra_event_putb)(&arg1,&arg2))
#define FlushBits()          (FNAME(compress_zebra_event_flushb)())

/* Space for literal arguments to fortran routines. */

static int arg1, arg2;       

/*
 * These are the two global data structures used in this program.
 * The window[] array is exactly that, the window of previously seen
 * text, as well as the current look ahead text.  The tree[] structure
 * contains the binary tree of all of the strings in the window sorted
 * in order.
*/

static unsigned int index_bit_count;     /* Number of bits in window index. */
static unsigned int length_bit_count;    /* Number of bits in length count. */
static unsigned int window_size;         /* Size of window as determined by 
                                            index_bit_count. */
static unsigned int break_even;          /* Minimum string length minus one. */
static unsigned int look_ahead_size;     /* Size of look ahead buffer as 
                                            determined by length_bit_count and
                                            break_even. */
static unsigned char *window = NULL;

static struct {
    int parent;
    int smaller_child;
    int larger_child;
} *tree = NULL;

/* Function prototypes for local routines. */

static void InitTree( int r );
static void ContractNode( int old_node, int new_node );
static void ReplaceNode( int old_node, int new_node );
static int FindNextNode( int node );
static void DeleteString( int p );
static int AddString( int new_node, int *match_position );

/* Function prototypes for fortran-callable c routines. */

void FNAME(compress_lzss_param)(int *index_bits, int *length_bits);
void FNAME(compress_lzss_ini)(void);
void FNAME(compress_lzss)(void);

/* Function prototypes for fortran routines called from c. */

int FNAME(compress_zebra_event_getc)(void);
int FNAME(compress_zebra_event_putb)(int *data, int *nbits);
int FNAME(compress_zebra_event_flushb)();

/*
 * To make the tree usable, a single phrase has to be added to the tree so 
 * it has a root node.  That is done right here.
*/
static void InitTree( int r )
{
    tree[ TREE_ROOT ].larger_child = r;
    tree[ r ].parent = TREE_ROOT;
    tree[ r ].larger_child = UNUSED;
    tree[ r ].smaller_child = UNUSED;
}

/*
 * This fortran-callable entry point is used to set compression parameters
 * and allocate space for the window and tree.
*/
void FNAME(compress_lzss_param)( int *index_bits, int *length_bits )
{

/* Save arguments in global variables. */

    index_bit_count = *index_bits;
    length_bit_count = *length_bits;

/* Other compression parameters are calculated here. */

    window_size = 1<<index_bit_count;
    break_even = ( 1 + index_bit_count + length_bit_count ) / 9;
    look_ahead_size = ( 1<<length_bit_count ) + break_even;

/* Allocate space for window. */

    if(window != NULL)
        free(window);
    window = calloc(window_size, sizeof(*window));

/* Allocate space for tree. */

    if(tree != NULL)
        free(tree);
    tree = calloc(window_size+1, sizeof(*tree));

/* Check for allocation errors. */

    if( window == NULL || tree == NULL ) {
        printf("Compress_lzss: malloc failed\n");
	abort();
    }
}
 
/*
 * This fortran-callable entry point re-initializes the tree to be empty.
*/
void FNAME(compress_lzss_ini)(void)
{
  int i;

  for(i=0; i<=window_size; i++) {
    tree[i].parent = UNUSED;
    tree[i].larger_child = UNUSED;
    tree[i].smaller_child = UNUSED;
  }
}

/*
 * This routine is used when a node is being deleted.  The link to
 * its descendant is broken by pulling the descendant in to overlay
 * the existing link.
 */
static void ContractNode( int old_node, int new_node )
{
    tree[ new_node ].parent = tree[ old_node ].parent;
    if ( tree[ tree[ old_node ].parent ].larger_child == old_node )
        tree[ tree[ old_node ].parent ].larger_child = new_node;
    else
        tree[ tree[ old_node ].parent ].smaller_child = new_node;
    tree[ old_node ].parent = UNUSED;
}

/*
 * This routine is also used when a node is being deleted.  However,
 * in this case, it is being replaced by a node that was not previously
 * in the tree.
 */
static void ReplaceNode( int old_node, int new_node )
{
    int parent;

    parent = tree[ old_node ].parent;
    if ( tree[ parent ].smaller_child == old_node )
        tree[ parent ].smaller_child = new_node;
    else
        tree[ parent ].larger_child = new_node;
    tree[ new_node ] = tree[ old_node ];
    tree[ tree[ new_node ].smaller_child ].parent = new_node;
    tree[ tree[ new_node ].larger_child ].parent = new_node;
    tree[ old_node ].parent = UNUSED;
}

/*
 * This routine is used to find the next smallest node after the node
 * argument.  It assumes that the node has a smaller child.  We find
 * the next smallest child by going to the smaller_child node, then
 * going to the end of the larger_child descendant chain.
*/
static int FindNextNode( int node )
{
    int next;

    next = tree[ node ].smaller_child;
    while ( tree[ next ].larger_child != UNUSED )
        next = tree[ next ].larger_child;
    return( next );
}

/*
 * This routine performs the classic binary tree deletion algorithm.
 * If the node to be deleted has a null link in either direction, we
 * just pull the non-null link up one to replace the existing link.
 * If both links exist, we instead delete the next link in order, which
 * is guaranteed to have a null link, then replace the node to be deleted
 * with the next link.
 */
static void DeleteString( int p )
{
    int  replacement;

    if ( tree[ p ].parent == UNUSED )
        return;
    if ( tree[ p ].larger_child == UNUSED )
        ContractNode( p, tree[ p ].smaller_child );
    else if ( tree[ p ].smaller_child == UNUSED )
        ContractNode( p, tree[ p ].larger_child );
    else {
        replacement = FindNextNode( p );
        DeleteString( replacement );
        ReplaceNode( p, replacement );
    }
}

/*
 * This where most of the work done by the encoder takes place.  This
 * routine is responsible for adding the new node to the binary tree.
 * It also has to find the best match among all the existing nodes in
 * the tree, and return that to the calling routine.  To make matters
 * even more complicated, if the new_node has a duplicate in the tree,
 * the old_node is deleted, for reasons of efficiency.
 */

static int AddString( int new_node, int *match_position )
{
    int i;
    int test_node;
    int delta;
    int match_length;
    int *child;

    if ( new_node == END_OF_STREAM )
        return( 0 );
    test_node = tree[ TREE_ROOT ].larger_child;
    match_length = 0;
    for ( ; ; ) {
        for ( i = 0 ; i < look_ahead_size ; i++ ) {
            delta = window[ MOD_WINDOW( new_node + i ) ] -
                    window[ MOD_WINDOW( test_node + i ) ];
            if ( delta != 0 )
                break;
        }
        if ( i >= match_length ) {
            match_length = i;
            *match_position = test_node;
            if ( match_length >= look_ahead_size ) {
                ReplaceNode( test_node, new_node );
                return( match_length );
            }
        }
        if ( delta >= 0 )
            child = &tree[ test_node ].larger_child;
        else
            child = &tree[ test_node ].smaller_child;
        if ( *child == UNUSED ) {
            *child = new_node;
            tree[ new_node ].parent = test_node;
            tree[ new_node ].larger_child = UNUSED;
            tree[ new_node ].smaller_child = UNUSED;
            return( match_length );
        }
        test_node = *child;
    }
}

/*
 * This is the compression routine.  It has to first load up the look
 * ahead buffer, then go into the main compression loop.  The main loop
 * decides whether to output a single character or an index/length
 * token that defines a phrase.  Once the character or phrase has been
 * sent out, another loop has to run.  The second loop reads in new
 * characters, deletes the strings that are overwritten by the new
 * character, then adds the strings that are created by the new
 * character.
 */

void FNAME(compress_lzss)( void )
{
    int i;
    int c;
    int look_ahead_bytes;
    static int current_position;
    int replace_count;
    int match_length;
    int match_position;
    int treedata;

/* If the tree is currently empty, reset the current position.  Otherwise,
   keep the current position from the previous call, but delete any phrases 
   that happen to overlap with any character in the look ahead buffer. */

    treedata = tree[TREE_ROOT].larger_child != UNUSED ||
               tree[TREE_ROOT].smaller_child != UNUSED;
    if( treedata )
        for ( i = -look_ahead_size+1 ; i < look_ahead_size ; i++ )
	    DeleteString(MOD_WINDOW(current_position + i));
    else
        current_position = 1;

/* Now fill look ahead buffer. */

    for ( i = 0 ; i < look_ahead_size ; i++ ) {
        if ( ( c = Getc(input) ) < 0 )
	    break;
	window[ MOD_WINDOW( current_position + i ) ]
	  = (unsigned char) c;
    }
    look_ahead_bytes = i;

/* Now either initialize an empty tree with the just-read phrase or add it to
   an existing tree. */

    if( treedata ) 
        match_length = AddString( current_position , &match_position );
    else {
        InitTree(current_position);
	match_position = 0;
	match_length = 0;
    }

/* Store the token sizes and current position for the uncompress routine. */

    OutputBits( output, (unsigned long) index_bit_count, 5 );
    OutputBits( output, (unsigned long) length_bit_count, 5 );
    OutputBits( output, (unsigned long) break_even, 4 );
    OutputBits( output, (unsigned long) current_position, index_bit_count );

/* Main loop. */

    while ( look_ahead_bytes > 0 ) {
        if ( match_length > look_ahead_bytes )
            match_length = look_ahead_bytes;
        if ( match_length <= break_even ) {
            replace_count = 1;
            OutputBit( output, 1 );
            OutputBits( output,
                        (unsigned long) window[ current_position ], 8 );
        } else {
            OutputBit( output, 0 );
            OutputBits( output,
                        (unsigned long) match_position, index_bit_count );
            OutputBits( output,
                        (unsigned long) ( match_length - ( break_even + 1 ) ),
                        length_bit_count );
            replace_count = match_length;
        }
        for ( i = 0 ; i < replace_count ; i++ ) {
            DeleteString( MOD_WINDOW( current_position + look_ahead_size ) );
            if ( ( c = Getc(input) ) < 0 )
                look_ahead_bytes--;
            else
                window[ MOD_WINDOW( current_position + look_ahead_size ) ]
                        = (unsigned char) c;
            current_position = MOD_WINDOW( current_position + 1 );
            if ( look_ahead_bytes )
                match_length = AddString( current_position, &match_position );
        }
    };
    OutputBit( output, 0 );
    OutputBits( output, (unsigned long) END_OF_STREAM, index_bit_count );

/* Flush output buffer. */

    FlushBits();
}

/************************** End of LZSS.C *************************/

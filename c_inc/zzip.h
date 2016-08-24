/* zzip.h -- hacks for zzip
 *
 * This file also contains the gzip header files tailor.h and gzip.h.
 *
 * 10/93 sss
 */

#ifndef __ZZIP
#define __ZZIP

/************************* begin tailor.h ********************************/
/* tailor.h -- target dependent definitions
 * Copyright (C) 1992-1993 Jean-loup Gailly.
 * This is free software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License, see the file COPYING.
 */

/* The target dependent definitions should be defined here only.
 * The target dependent functions should be defined in tailor.c.
 */

/* $Id$ */

#if defined(__MSDOS__) && !defined(MSDOS)
#  define MSDOS
#endif

#if defined(__OS2__) && !defined(OS2)
#  define OS2
#endif

#if defined(OS2) && defined(MSDOS) /* MS C under OS/2 */
#  undef MSDOS
#endif

#ifdef MSDOS
#  ifdef __GNUC__
     /* DJGPP version 1.09+ on MS-DOS.
      * The DJGPP 1.09 stat() function must be upgraded before gzip will
      * fully work.
      * No need for DIRENT, since <unistd.h> defines POSIX_SOURCE which
      * implies DIRENT.
      */
#    define near
#  else
#    define MAXSEG_64K
#    ifdef __TURBOC__
#      define NO_OFF_T
#      ifdef __BORLANDC__
#        define DIRENT
#      else
#        define NO_UTIME
#      endif
#    else /* MSC */
#      define HAVE_SYS_UTIME_H
#      define NO_UTIME_H
#    endif
#  endif
#  define PATH_SEP2 '\\'
#  define PATH_SEP3 ':'
#  define MAX_PATH_LEN  128
#  define NO_MULTIPLE_DOTS
#  define MAX_EXT_CHARS 3
#  define Z_SUFFIX "z"
#  define NO_CHOWN
#  define PROTO
#  define STDC_HEADERS
#  define NO_SIZE_CHECK
#  define casemap(c) tolow(c) /* Force file names to lower case */
#  include <io.h>
#  define OS_CODE  0x00
#  define SET_BINARY_MODE(fd) setmode(fd, O_BINARY)
#  if !defined(NO_ASM) && !defined(ASMV)
#    define ASMV
#  endif
#else
#  define near
#endif

#ifdef OS2
#  define PATH_SEP2 '\\'
#  define PATH_SEP3 ':'
#  define MAX_PATH_LEN  260
#  ifdef OS2FAT
#    define NO_MULTIPLE_DOTS
#    define MAX_EXT_CHARS 3
#    define Z_SUFFIX "z"
#    define casemap(c) tolow(c)
#  endif
#  define NO_CHOWN
#  define PROTO
#  define STDC_HEADERS
#  include <io.h>
#  define OS_CODE  0x06
#  define SET_BINARY_MODE(fd) setmode(fd, O_BINARY)
#  ifdef _MSC_VER
#    define HAVE_SYS_UTIME_H
#    define NO_UTIME_H
#    define MAXSEG_64K
#    undef near
#    define near _near
#  endif
#  ifdef __EMX__
#    define HAVE_SYS_UTIME_H
#    define NO_UTIME_H
#    define DIRENT
#    define EXPAND(argc,argv) \
       {_response(&argc, &argv); _wildcard(&argc, &argv);}
#  endif
#  ifdef __BORLANDC__
#    define DIRENT
#  endif
#  ifdef __ZTC__
#    define NO_DIR
#    define NO_UTIME_H
#    include <dos.h>
#    define EXPAND(argc,argv) \
       {response_expand(&argc, &argv);}
#  endif
#endif

#ifdef WIN32 /* Windows NT */
#  define HAVE_SYS_UTIME_H
#  define NO_UTIME_H
#  define PATH_SEP2 '\\'
#  define PATH_SEP3 ':'
#  define MAX_PATH_LEN  260
#  define NO_CHOWN
#  define PROTO
#  define STDC_HEADERS
#  define SET_BINARY_MODE(fd) setmode(fd, O_BINARY)
#  include <io.h>
#  include <malloc.h>
#  ifdef NTFAT
#    define NO_MULTIPLE_DOTS
#    define MAX_EXT_CHARS 3
#    define Z_SUFFIX "z"
#    define casemap(c) tolow(c) /* Force file names to lower case */
#  endif
#  define OS_CODE  0x0b
#endif

#ifdef MSDOS
#  ifdef __TURBOC__
#    include <alloc.h>
#    define DYN_ALLOC
     /* Turbo C 2.0 does not accept static allocations of large arrays */
     void * fcalloc (unsigned items, unsigned size);
     void fcfree (void *ptr);
#  else /* MSC */
#    include <malloc.h>
#    define fcalloc(nitems,itemsize) halloc((long)(nitems),(itemsize))
#    define fcfree(ptr) hfree(ptr)
#  endif
#else
#  ifdef MAXSEG_64K
#    define fcalloc(items,size) calloc((items),(size))
#  else
#    define fcalloc(items,size) malloc((size_t)(items)*(size_t)(size))
#  endif
#  define fcfree(ptr) free(ptr)
#endif

#if defined(VAXC) || defined(VMS)
#  define PATH_SEP ']'
#  define PATH_SEP2 ':'
#  define SUFFIX_SEP ';'
#  define NO_MULTIPLE_DOTS
#  define Z_SUFFIX "-gz"
#  define RECORD_IO 1
#  define casemap(c) tolow(c)
#  define OS_CODE  0x02
#  define OPTIONS_VAR "GZIP_OPT"
#  define STDC_HEADERS
#  define NO_UTIME
#  define EXPAND(argc,argv) vms_expand_args(&argc,&argv);
#  include <file.h>
#  define unlink delete
#  ifdef VAXC
#    define NO_FCNTL_H
#    include <unixio.h>
#  endif
#endif

#ifdef AMIGA
#  define PATH_SEP2 ':'
#  define STDC_HEADERS
#  define OS_CODE  0x01
#  define ASMV
#  ifdef __GNUC__
#    define DIRENT
#    define HAVE_UNISTD_H
#  else /* SASC */
#    define NO_STDIN_FSTAT
#    define SYSDIR
#    define NO_SYMLINK
#    define NO_CHOWN
#    define NO_FCNTL_H
#    include <fcntl.h> /* for read() and write() */
#    define direct dirent
     extern void _expand_args(int *argc, char ***argv);
#    define EXPAND(argc,argv) _expand_args(&argc,&argv);
#    undef  O_BINARY /* disable useless --ascii option */
#  endif
#endif

#if defined(ATARI) || defined(atarist)
#  ifndef STDC_HEADERS
#    define STDC_HEADERS
#    define HAVE_UNISTD_H
#    define DIRENT
#  endif
#  define ASMV
#  define OS_CODE  0x05
#  ifdef TOSFS
#    define PATH_SEP2 '\\'
#    define PATH_SEP3 ':'
#    define MAX_PATH_LEN  128
#    define NO_MULTIPLE_DOTS
#    define MAX_EXT_CHARS 3
#    define Z_SUFFIX "z"
#    define NO_CHOWN
#    define casemap(c) tolow(c) /* Force file names to lower case */
#    define NO_SYMLINK
#  endif
#endif

#ifdef MACOS
#  define PATH_SEP ':'
#  define DYN_ALLOC
#  define PROTO
#  define NO_STDIN_FSTAT
#  define NO_CHOWN
#  define NO_UTIME
#  define chmod(file, mode) (0)
#  define OPEN(name, flags, mode) open(name, flags)
#  define OS_CODE  0x07
#  ifdef MPW
#    define isatty(fd) ((fd) <= 2)
#  endif
#endif

#ifdef __50SERIES /* Prime/PRIMOS */
#  define PATH_SEP '>'
#  define STDC_HEADERS
#  define NO_MEMORY_H
#  define NO_UTIME_H
#  define NO_UTIME
#  define NO_CHOWN 
#  define NO_STDIN_FSTAT 
#  define NO_SIZE_CHECK 
#  define NO_SYMLINK
#  define RECORD_IO  1
#  define casemap(c)  tolow(c) /* Force file names to lower case */
#  define put_char(c) put_byte((c) & 0x7F)
#  define get_char(c) ascii2pascii(get_byte())
#  define OS_CODE  0x0F    /* temporary, subject to change */
#  ifdef SIGTERM
#    undef SIGTERM         /* We don't want a signal handler for SIGTERM */
#  endif
#endif

#if defined(pyr) && !defined(NOMEMCPY) /* Pyramid */
#  define NOMEMCPY /* problem with overlapping copies */
#endif

#ifdef TOPS20
#  define OS_CODE  0x0a
#endif

#ifndef unix
#  define NO_ST_INO /* don't rely on inode numbers */
#endif


	/* Common defaults */

#ifndef OS_CODE
#  define OS_CODE  0x03  /* assume Unix */
#endif

#ifndef PATH_SEP
#  define PATH_SEP '/'
#endif

#ifndef casemap
#  define casemap(c) (c)
#endif

#ifndef OPTIONS_VAR
#  define OPTIONS_VAR "GZIP"
#endif

#ifndef Z_SUFFIX
#  define Z_SUFFIX ".gz"
#endif

#ifdef MAX_EXT_CHARS
#  define MAX_SUFFIX  MAX_EXT_CHARS
#else
#  define MAX_SUFFIX  30
#endif

#ifndef MAKE_LEGAL_NAME
#  ifdef NO_MULTIPLE_DOTS
#    define MAKE_LEGAL_NAME(name)   make_simple_name(name)
#  else
#    define MAKE_LEGAL_NAME(name)
#  endif
#endif

#ifndef MIN_PART
#  define MIN_PART 3
   /* keep at least MIN_PART chars between dots in a file name. */
#endif

#ifndef EXPAND
#  define EXPAND(argc,argv)
#endif

#ifndef RECORD_IO
#  define RECORD_IO 0
#endif

#ifndef SET_BINARY_MODE
#  define SET_BINARY_MODE(fd)
#endif

#ifndef OPEN
#  define OPEN(name, flags, mode) open(name, flags, mode)
#endif

#ifndef get_char
#  define get_char() get_byte()
#endif

#ifndef put_char
#  define put_char(z,c) put_byte(z,c)
#endif


/************************* end tailor.h ********************************/

/************************* begin gzip.h part 1  ************************/


/* gzip.h -- common declarations for all gzip modules
 * Copyright (C) 1992-1993 Jean-loup Gailly.
 * This is free software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License, see the file COPYING.
 *
 * Hacked for zzip 10/93  sss
 */

#if defined(__STDC__) || defined(PROTO)
#  define OF(args)  args
#else
#  define OF(args)  ()
#endif

#ifdef __STDC__
   typedef void *voidp;
#else
   typedef char *voidp;
#endif

/* I don't like nested includes, but the string and io functions are used
 * too often
 */
#include <stdio.h>
#if !defined(NO_STRING_H) || defined(STDC_HEADERS)
#  include <string.h>
#  if !defined(STDC_HEADERS) && !defined(NO_MEMORY_H) && !defined(__GNUC__)
#    include <memory.h>
#  endif
#  define memzero(s, n)     memset ((voidp)(s), 0, (n))
#else
#  include <strings.h>
#  define strchr            index 
#  define strrchr           rindex
#  define memcpy(d, s, n)   bcopy((s), (d), (n)) 
#  define memcmp(s1, s2, n) bcmp((s1), (s2), (n)) 
#  define memzero(s, n)     bzero((s), (n))
#endif

#ifndef RETSIGTYPE
#  define RETSIGTYPE void
#endif

#define local static

typedef unsigned char  uch;
typedef unsigned short ush;
typedef unsigned long  ulg;

/*************************   end gzip.h part 1  ************************/

/* zzip header definitions */

#define ZZIP_MAGIC "zzip"
#define ZZIP_MAGIC_LENGTH 4

#define ZTAG_MAGIC "ztag"
#define ZTAG_MAGIC_LENGTH 4

#define PACK_SIZE 4


struct block_record {
  /* Position of the block in the uncompressed file. */
  ulg block_start;

  /* Position of the block in the compressed file. */
  ulg zblock_start;
};


/* Table of block correspondences. */
struct block_table {
  ulg nblocks;
  ulg nblocks_allocated;
  struct block_record *blocks;
};


/* Define descriptors for compression and uncompression.
   These descriptors hold (pointers to) the entire current state
   of (de)compression. */


struct zzip_desc {
  int ofd;
  char *fname;
  int first;
  char *inbuf_ptr;
  int  inbuf_left;
  int outcnt;
  int level;
  uch *l_buf;
  uch *outbuf;
  ush *d_buf;
  uch *window;
  ulg window_size;
  ush *prev;
  ush *head;
  long block_start;
  ulg strstart;

  /* Position in the uncompressed input file of the last block. */
  ulg last_block_start;

  /* (bit) Position in the compressed output file of the last block. */
  ulg zlast_block_start;
  ulg zheader_length; 

  struct bi_info *bi;
  struct deflate_info *de;
  struct ct_info *ct;

  /* Table of input/output block correspondences. */
  struct block_table b;
};


struct zunzip_desc {
  int ifd;
  char *fname;
  char *zoutbuf;
  int zoutbuf_len;
  char *zoutbuf_ptr;
  ulg bytes_to_skip;
  int zeofile;
  int zoutbuf_size;
  int insize;
  int inptr;
  int outcnt;
  uch *inbuf;
  uch *slide;
  ulg bb;
  unsigned bk;

  /* Table of input/output block correspondences. */
  struct block_table b;
};


/* Alias all private names so they start with _zzip. */

#define bi_init        _zzip_bi_init
#define send_bits      _zzip_send_bits
#define bi_reverse     _zzip_bi_reverse
#define bi_windup      _zzip_bi_windup
#define copy_block     _zzip_copy_block
#define lm_init        _zzip_lm_init
#define deflate        _zzip_deflate
#define inflate_block  _zzip_inflate_block
#define inflate        _zzip_inflate
#define inflate_init   _zzip_inflate_init
#define inflate_finish _zzip_inflate_finish
#define ct_init        _zzip_ct_init
#define ct_finish      _zzip_ct_finish
#define flush_block    _zzip_flush_block
#define ct_tally       _zzip_ct_tally
#define xmalloc        _zzip_xmalloc
#define xrealloc       _zzip_xrealloc
#define read_buf       _zzip_read_buf
#define error          _zzip_error
#define warn           _zzip_warn
#define flush_outbuf   _zzip_flush_outbuf
#define flush_window   _zzip_flush_window
#define fill_inbuf     _zzip_fill_inbuf
#define record_block   _zzip_record_block
#define write_int_to_zip  _zzip_write_int_to_zip
#define read_int_from_zip _zzip_read_int_from_zip
#define inflate_skip_bits _zzip_inflate_skip_bits


/* zzip.c routines */
extern struct zzip_desc *_zzip_open (char *fname, int pack_level);
extern void _zzip_block (struct zzip_desc *z, char *buf, int size);
extern void _zzip_close (struct zzip_desc *z);
extern int  _zzip_write (struct zzip_desc *z, char* buf, int len);
extern void record_block (struct zzip_desc *z, ulg total_zip_length,
			  ulg this_block_len);

extern struct zunzip_desc *_zunzip_open (char *fname);
extern int _zunzip_block (struct zunzip_desc *z, char *buf, int size);
extern void _zunzip_close (struct zunzip_desc *z);
extern void _zunzip_seek (struct zunzip_desc *z, ulg pos);

extern void write_int_to_zip (struct zzip_desc *z, ulg x);
extern  ulg read_int_from_zip (struct zunzip_desc *z);


/* Some more convenient notation for properly calling xmalloc. */

#include <stddef.h>

extern void *xmalloc (size_t size);
extern void *xrealloc (void *x, size_t size);

#define XALLOC(type)    (type *)xmalloc (sizeof (type))
#define NALLOC(type, n) (type *)xmalloc (sizeof (type) * (n))
#define NREALLOC(type, x, n) (type *)xrealloc (x, sizeof (type) * (n))


/************************* begin gzip.h part 2 *****************************/


/* Return codes from gzip */
#define OK      0
#define ERROR   1
#define WARNING 2

/* Compression methods (see algorithm.doc) */
#define STORED      0
#define COMPRESSED  1
#define PACKED      2
#define LZHED       3
/* methods 4 to 7 reserved */
#define DEFLATED    8
#define MAX_METHODS 9

#ifndef	INBUFSIZ
#  ifdef SMALL_MEM
#    define INBUFSIZ  0x2000  /* input buffer size */
#  else
#    define INBUFSIZ  0x8000  /* input buffer size */
#  endif
#endif
#define INBUF_EXTRA  64     /* required by unlzw() */

#ifndef	OUTBUFSIZ
#  ifdef SMALL_MEM
#    define OUTBUFSIZ   8192  /* output buffer size */
#  else
#    define OUTBUFSIZ  16384  /* output buffer size */
#  endif
#endif
#define OUTBUF_EXTRA 2048   /* required by unlzw() */

#ifndef DIST_BUFSIZE
#  ifdef SMALL_MEM
#    define DIST_BUFSIZE 0x2000 /* buffer for distances, see trees.c */
#  else
#    define DIST_BUFSIZE 0x8000 /* buffer for distances, see trees.c */
#  endif
#endif


typedef int file_t;     /* Do not use stdio */
#define NO_FILE  (-1)   /* in memory compression */


#define	PACK_MAGIC     "\037\036" /* Magic header for packed files */
#define	GZIP_MAGIC     "\037\213" /* Magic header for gzip files, 1F 8B */
#define	OLD_GZIP_MAGIC "\037\236" /* Magic header for gzip 0.5 = freeze 1.x */
#define	LZH_MAGIC      "\037\240" /* Magic header for SCO LZH Compress files*/
#define PKZIP_MAGIC    "\120\113\003\004" /* Magic header for pkzip files */

/* gzip flag byte */
#define ASCII_FLAG   0x01 /* bit 0 set: file probably ascii text */
#define CONTINUATION 0x02 /* bit 1 set: continuation of multi-part gzip file */
#define EXTRA_FIELD  0x04 /* bit 2 set: extra field present */
#define ORIG_NAME    0x08 /* bit 3 set: original file name present */
#define COMMENT      0x10 /* bit 4 set: file comment present */
#define ENCRYPTED    0x20 /* bit 5 set: file is encrypted */
#define RESERVED     0xC0 /* bit 6,7:   reserved */

/* internal file attribute */
#define UNKNOWN 0xffff
#define BINARY  0
#define ASCII   1

#ifndef WSIZE
#  define WSIZE 0x8000     /* window size--must be a power of two, and */
#endif                     /*  at least 32K for zip's deflate method */

#define MIN_MATCH  3
#define MAX_MATCH  258
/* The minimum and maximum match lengths */

#define MIN_LOOKAHEAD (MAX_MATCH+MIN_MATCH+1)
/* Minimum amount of lookahead, except at the end of the input file.
 * See deflate.c for comments about the MIN_MATCH+1.
 */

#define MAX_DIST  (WSIZE-MIN_LOOKAHEAD)
/* In order to simplify the code, particularly on 16 bit machines, match
 * distances are limited to MAX_DIST instead of WSIZE.
 */

#define get_byte(z)  (z->inptr < z->insize ? z->inbuf[z->inptr++] : fill_inbuf(z, 0))
#define try_byte(z)  (z->inptr < z->insize ? z->inbuf[z->inptr++] : fill_inbuf(z, 1))

/* put_byte is used for the compressed output, put_ubyte for the
 * uncompressed output. However unlzw() uses window for its
 * suffix table instead of its output buffer, so it does not use put_ubyte
 * (to be cleaned up).
 */
#define put_byte(z,c) {z->outbuf[z->outcnt++]=(uch)(c); if (z->outcnt==OUTBUFSIZ)\
   flush_outbuf(z);}
#define put_ubyte(z,c) {z->window[z->outcnt++]=(uch)(c); if (z->outcnt==WSIZE)\
   flush_window(z);}

/* Output a 16 bit value, lsb first */
#define put_short(z,w) \
{ if (z->outcnt < OUTBUFSIZ-2) { \
    z->outbuf[z->outcnt++] = (uch) ((w) & 0xff); \
    z->outbuf[z->outcnt++] = (uch) ((ush)(w) >> 8); \
  } else { \
    put_byte(z,(uch)((w) & 0xff)); \
    put_byte(z,(uch)((ush)(w) >> 8)); \
  } \
}

/* Output a 32 bit value to the bit stream, lsb first */
#define put_long(z,n) { \
    put_short(z,(n) & 0xffff); \
    put_short(z,((ulg)(n)) >> 16); \
}

#define seekable()    0  /* force sequential output */
#define translate_eol 0  /* no option -a yet */

#define tolow(c)  (isupper(c) ? (c)-'A'+'a' : (c))    /* force to lower case */

/* Macros for getting two-byte and four-byte header values */
#define SH(p) ((ush)(uch)((p)[0]) | ((ush)(uch)((p)[1]) << 8))
#define LG(p) ((ulg)(SH(p)) | ((ulg)(SH((p)+2)) << 16))

/* Diagnostic functions */
#ifdef DEBUG
#  define Assert(cond,msg) {if(!(cond)) error(msg);}
#  define Trace(x) fprintf x
#  define Tracev(x) {if (verbose) fprintf x ;}
#  define Tracevv(x) {if (verbose>1) fprintf x ;}
#  define Tracec(c,x) {if (verbose && (c)) fprintf x ;}
#  define Tracecv(c,x) {if (verbose>1 && (c)) fprintf x ;}
#else
#  define Assert(cond,msg)
#  define Trace(x)
#  define Tracev(x)
#  define Tracevv(x)
#  define Tracec(c,x)
#  define Tracecv(c,x)
#endif

#define WARN(msg) {if (!quiet) fprintf msg ; \
		   if (exit_code == OK) exit_code = WARNING;}

	/* in zip.c: */
extern int zip        OF((int in, int out));
extern int file_read  OF((char *buf,  unsigned size));

	/* in unzip.c */
extern int unzip      OF((int in, int out));
extern int check_zipfile OF((int in));

	/* in unpack.c */
extern int unpack     OF((int in, int out));

	/* in unlzh.c */
extern int unlzh      OF((int in, int out));

	/* in gzip.c */
RETSIGTYPE abort_gzip OF((void));

        /* in deflate.c */
void lm_init OF((struct zzip_desc *z, int pack_level, ush *flags));
ulg  deflate OF((struct zzip_desc *z, int last_block /* sss */));

        /* in trees.c */
void ct_init     OF((struct zzip_desc *z, ush *attr, int *method));
int  ct_tally    OF((struct zzip_desc *z, int dist, int lc));
ulg  flush_block OF((struct zzip_desc *z, char *buf, ulg stored_len, int eof));
void ct_finish   OF((struct zzip_desc *z));

        /* in bits.c */
void     bi_init    OF((struct zzip_desc *z));
void     send_bits  OF((struct zzip_desc *z, int value, int length));
unsigned bi_reverse OF((unsigned value, int length));
void     bi_windup  OF((struct zzip_desc *z));
void     copy_block OF((struct zzip_desc *z, char *buf, unsigned len, int header));
extern   int (*read_buf) OF((struct zzip_desc *z, char *buf, unsigned size));

	/* in util.c: */
extern int copy           OF((int in, int out));
extern ulg  updcrc        OF((uch *s, unsigned n));
extern void clear_bufs    OF((void));
extern int  fill_inbuf    OF((struct zunzip_desc *z, int eof_ok));
extern void flush_outbuf  OF((struct zzip_desc *z));
extern void flush_window  OF((struct zunzip_desc *z));
/*extern void write_buf     OF((int fd, voidp buf, unsigned cnt));*/
extern char *strlwr       OF((char *s));
/*extern char *basename     OF((char *fname));*/
extern void make_simple_name OF((char *name));
extern char *add_envopt   OF((int *argcp, char ***argvp, char *env));
extern void error         OF((char *m));
extern void warn          OF((char *a, char *b));
extern void read_error    OF((void));
extern void write_error   OF((void));
extern void display_ratio OF((long num, long den, FILE *file));
/*extern voidp xmalloc      OF((unsigned int size));*/

	/* in inflate.c */
extern int inflate OF((struct zunzip_desc *z));
extern int inflate_block  OF((struct zunzip_desc *z, int *));
extern void inflate_init   OF((struct zunzip_desc *z));
extern void inflate_finish OF((struct zunzip_desc *z));
extern void inflate_skip_bits OF((struct zunzip_desc *z, int nbits));

/************************* end gzip.h part 2 *****************************/

#endif

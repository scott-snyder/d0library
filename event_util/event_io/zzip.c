/* zzip.c -- driver functions for blockwise compression/decompression.
 *
 * 10/93 sss
 *
 * This file contains the low-level interfaces to the gzip routines.
 * The higher-level code for interfacing this with Zebra is contained
 * in zzip_interface.c
 *
 * Exported functions:
 *
 * struct zzip_desc *_zzip_open (char *fname, int pack_level)
 *
 *   Opens a new file for writing compressed data.
 *   PACK_LEVEL is in integer in the range 1-9 specifying the amount
 *   of compression desired.  If successful, this routine returns
 *   a pointer to a structure describing the state of the compressor.
 *   If the open fails, this routine returns NULL.
 *
 * void _zzip_block (struct zzip_desc *z, char *buf, int size)
 *
 *   Takes the block consisting of SIZE bytes in BUF, compresses it,
 *   and writes it to the output file described by Z.
 *
 * int _zzip_write (struct zzip_desc *z, char *buf, int len)
 *
 *   Writes the LEN bytes in BUF to the output file Z immediately,
 *   without compression.  Used for writing the zzip file header.
 *
 * void _zzip_write_int_to_zip (struct zzip_desc *z, ulg x)
 *
 *    Writes the 4-byte integer X to the output file Z in a portable
 *    format immediately, without compression.  Used for writing the
 *    zzip file header.
 *
 * void _zzip_close (struct zzip_desc *z)
 *
 *   Closes the output file described by Z, flushing all buffers and
 *   releasing allocated memory.
 *
 *
 * struct zunzip_desc *_zunzip_open (char *fname)
 *
 *   Opens the file FNAME for reading compressed data.
 *   If successful, this routine returns
 *   a pointer to a structure describing the state of the decompressor.
 *   If the open fails, this routine returns NULL.
 *
 * int _zunzip_block (struct zunzip_desc *z, char *buf, int size)
 *
 *   Try to read SIZE uncompressed bytes from the file Z into BUF.
 *   Returns the number of bytes actually transferred.
 *   At EOF, the returned value is 0.
 *
 * void _zunzip_seek (struct zunzip_desc *z, ulg pos)
 *
 *   Seek in the compressed file Z so that the next byte returned
 *   by _zunzip_block will be at byte offset POS in the
 *   _uncompressed_ data stream.
 *
 * ulg read_int_from_zip (struct zunzip_desc *z)
 *
 *   Immediately reads a 4-byte integer from the file Z.
 *   The integer should be one originally written by _zzip_write_int_to_zip.
 *
 * void _zunzip_close (struct zunzip_desc *z)
 *
 *   Closes the input file described by Z,
 *   releasing allocated memory.
 */

#include <stdlib.h>
#include <stdio.h>
#ifdef VMS
#include <file.h>
#else
#include <fcntl.h>
#endif
#if defined(VMS) && !defined(__GNUC__)
#include <unixio.h>
#endif

#ifdef VMS
#define ZZIP_INC "d0$c_inc:zzip.h"
#else
#define ZZIP_INC "zzip.h"
#endif
#include ZZIP_INC

#ifndef S_IRUSR
#  define S_IRUSR 0400
#endif
#ifndef S_IWUSR
#  define S_IWUSR 0200
#endif
#define RW_USER (S_IRUSR | S_IWUSR)  /* creation mode for open() */

#ifndef S_IRGRP
#  define S_IRGRP 0040
#endif
#ifndef S_IWGRP
#  define S_IWGRP 0020
#endif
#define RW_GROUP (S_IRGRP | S_IWGRP)  /* creation mode for open() */

#ifndef S_IROTH
#  define S_IROTH 0004
#endif
#ifndef S_IWOTH
#  define S_IWOTH 0002
#endif
#define RW_OTHER (S_IROTH | S_IWOTH)  /* creation mode for open() */


#ifndef O_BINARY
#  define  O_BINARY  0  /* creation mode for open() */
#endif

#ifndef SEEK_SET
# define SEEK_SET 0
# define SEEK_CUR 1
# define SEEK_END 2
#endif

int (*read_buf) OF((struct zzip_desc *z, char *buf, unsigned size));

/*******************************************************************************
 * A few utility routines needed to support zzip.
 */

/* Print an error and die. */

void error (char *s)
{
  fprintf (stderr, "ZZIP ERROR: %s\n", s);
  abort ();
}


void warn (char *a, char *b)
{
}


/* Malloc a block; die if we can't get it. */

void *xmalloc (size_t size)
{
  extern void *malloc ();
  void *x = malloc (size);
  if (x == NULL)
    error ("malloc fails");
  return x;
}


void *xrealloc (void *x, size_t size)
{
  extern void *realloc ();
  x = realloc (x, size);
  if (x == NULL)
    error ("realloc fails");
  return x;
}


/* Functions to pack and unpack an integer into a string
   in an endian-independent manner. */

local
void pack_int (ulg x, uch out[PACK_SIZE])
{
  int i;
  int lim = sizeof (int);
  if (lim > PACK_SIZE) lim = PACK_SIZE;
  for (i=0; i<PACK_SIZE; i++) {
    if (i < lim) {
      out[i] = x & 0xff;
      x >>= 8;
    }
    else
      out[i] = 0;
  }
}


local
ulg unpack_int (uch out[PACK_SIZE])
{
  ulg r = 0;
  int i;
  for (i=PACK_SIZE-1; i>=0; i--)
    r = (r<<8) + out[i];
  return r;
}


/******************************************************************************
 * Compression (output).
 */


/* Read a block of uncompressed data from the input `file'.
   In this case, it comes from the output buffer which Zebra supplied. */

local int zzip_read (struct zzip_desc *z, char *buf, unsigned int size)
{
  unsigned int this_size = size;

  /* Stop reading at the end of the buffer. */
  if (this_size > z->inbuf_left)
    this_size = z->inbuf_left;

  /* Copy from the Zebra buffer to deflate's sliding window. */
  if (this_size > 0) {
    memcpy (buf, z->inbuf_ptr, this_size);
    z->inbuf_left -= this_size;
    z->inbuf_ptr += this_size;
  }

  return this_size;
}


/* Write a buffer to FD using unix io.
   Return -1 if there's an error. */

local
int write_buf(fd, buf, cnt)
    int       fd;
    voidp     buf;
    unsigned  cnt;
{
  unsigned  n;

  while ((n = write(fd, buf, cnt)) != cnt) {
    if (n == (unsigned)(-1))
      return -1;
    cnt -= n;
    buf = (voidp)((char*)buf+n);
  }
  return 0;
}


local void do_zip_write (struct zzip_desc *z, char *buf, ulg len)
{
  if (write_buf (z->ofd, buf, len) < 0) {
    /* Die on write errors. */
    perror (z->fname);
    error ("writing output");
  }
}


/* Write header, etc. information to the compressed file. */

int _zzip_write (struct zzip_desc *z, char *buf, int len)
{
  do_zip_write (z, buf, len);
  z->zlast_block_start += len * 8;
  return len;
}


void write_int_to_zip (struct zzip_desc *z, ulg x)
{
  uch buf[PACK_SIZE];
  pack_int (x, buf);
  _zzip_write (z, (char *)buf, PACK_SIZE);
}


/* Called by deflate to write out and empty the output buffer. */

void flush_outbuf (struct zzip_desc *z)
{
  /* If the buffer is empty, there's nothing to do. */
  if (z->outcnt == 0) return;

  /* Write the buffer. */
  do_zip_write (z, (char *)z->outbuf, z->outcnt);

  /* The buffer is now empty. */
  z->outcnt = 0;
}


/* Open the file FNAME for writing compressed output.
   PACK_LEVEL is the degree of compression desired (0-9).
   Returns a pointer to a descriptor which must be provided
   to the other zzip routines.  Returns NULL if the output file
   couldn't be opened. */

struct zzip_desc *_zzip_open (char *fname, int pack_level)
{
  int fd;
  struct zzip_desc *z;

  /* Open the output file. */
  fd = creat (fname, 
	      RW_USER | RW_GROUP | RW_OTHER
#ifdef VMS
	     /* vax c usually defaults to this, but not if there's
		an existing file of the same name... */
	     , "rfm=stmlf"
#endif
	     );
  if (fd == -1) {
    return NULL;
  }

  /* Allocate the descriptor and store the file descriptor and filename. */
  z = XALLOC (struct zzip_desc);
  z->ofd = fd;
  z->fname = NALLOC (char, strlen (fname)+1);
  strcpy (z->fname, fname);

  /* Allocate working space needed by deflate. */
  z->l_buf  = NALLOC (uch, INBUFSIZ);
  z->outbuf = NALLOC (uch, OUTBUFSIZ+OUTBUF_EXTRA);
  z->d_buf  = NALLOC (ush, DIST_BUFSIZE);
  z->head   = NALLOC (ush, WSIZE);
  z->prev   = NALLOC (ush, WSIZE);

  z->window = NALLOC (uch, 2*WSIZE);
  z->window_size = 2*WSIZE;

  /* Initialize everything we can at this point. */
  bi_init (z);
  z->outcnt = 0;
  z->level = pack_level;
  read_buf = zzip_read;

  z->last_block_start = 0;
  z->zlast_block_start = 0;
  z->zheader_length = 0;
  z->b.nblocks = z->b.nblocks_allocated = 0;
  z->b.blocks = NULL;

  z->first = 1;

  return z;
}


/* Take one block of data and send it through the compressor to the
   output file. */

void _zzip_block (struct zzip_desc *z, char *buf, int size)
{
  ush flags;
  ush attr = BINARY;
  int method = DEFLATED;

  /* Set up the pointers to the input block. */
  z->inbuf_ptr = buf;
  z->inbuf_left = size;

  /* If this is the first block, initialize deflate. */
  if (z->first) {
    z->zheader_length = z->zlast_block_start;
    lm_init (z, z->level, &flags);
    ct_init (z, &attr, &method);
    z->first = 0;
  }

  /* Process the block. */
  deflate (z, 0);
}


/* Write the block table to the tail of the file. */

local void dump_block_table (struct zzip_desc *z)
{
  int i;
  ulg block_table_pos = z->zlast_block_start >> 3;

  /* A tag to identify the block table. */
  do_zip_write (z, ZTAG_MAGIC, ZTAG_MAGIC_LENGTH);

  /* Count of blocks. */
  write_int_to_zip (z, z->b.nblocks);

  /* Dump the block table. */
  for (i=0; i<z->b.nblocks; i++) {
    write_int_to_zip (z, z->b.blocks[i].block_start);
    write_int_to_zip (z, z->b.blocks[i].zblock_start);
  }

  /* Finish off with a pointer to the start of the block table. */
  write_int_to_zip (z, block_table_pos);
}


/* Finish off the file: Flush anything that's left to be done, close
   the output file, and free resources. */

void _zzip_close (struct zzip_desc *z)
{
  /* Finish compressing the tail of the file and flush the output. */
  if (!z->first) {
    z->inbuf_left = 0;
    deflate (z, 1);
    flush_outbuf (z);
    ct_finish (z);
    free (z->de);
  }

  dump_block_table (z);

  /* Close the file. */
  close (z->ofd);

  /* Free all the memory we've allocated. */
  if (z->b.blocks != NULL) free (z->b.blocks);
  free (z->fname);
  free (z->bi);
  free (z->prev);
  free (z->head);
  free (z->d_buf);
  free (z->l_buf);
  free (z->outbuf);
  free (z->window);
  free (z);
}


/* Add another block correspondence to this file's record. */

local void add_block_record (struct zzip_desc *z,
			     ulg zblock_start, ulg block_start)
{
  if (z->b.nblocks >= z->b.nblocks_allocated) {
    if (z->b.nblocks_allocated > 0) {
      z->b.nblocks_allocated *= 2;
      z->b.blocks = NREALLOC (struct block_record, z->b.blocks,
			      z->b.nblocks_allocated);
    }
    else {
      z->b.nblocks_allocated = 100;
      z->b.blocks = NALLOC (struct block_record, z->b.nblocks_allocated);
    }
  }

  z->b.blocks[z->b.nblocks].zblock_start = zblock_start;
  z->b.blocks[z->b.nblocks].block_start  =  block_start;
  z->b.nblocks++;
}


/* Called by flush_block after each block is written.
   TOTAL_ZIP_LENGTH is the total number of bytes written to the output
   file so far.
   THIS_BLOCK_LEN is the uncompressed length of the block which
   was just written. */

void record_block (struct zzip_desc *z, ulg total_zip_length,
		   ulg this_block_len)
{
  add_block_record (z, z->zlast_block_start, z->last_block_start);
  z->zlast_block_start = total_zip_length + z->zheader_length;
  z->last_block_start += this_block_len;
}


/******************************************************************************
 * Decompression (input).
 */


#define ZOUTBUF_SIZE 65536*4

ulg read_int_from_zip (struct zunzip_desc *z)
{
  uch buf[PACK_SIZE];
  if (read (z->ifd, buf, PACK_SIZE) != PACK_SIZE)
    error ("reading input file");
  return unpack_int (buf);
}


local
void read_block_table (struct zunzip_desc *z)
{
  int i;
  ulg tag_pointer;
  char magic_buf[ZTAG_MAGIC_LENGTH];

  /* Return if it's already there. */
  if (z->b.blocks != NULL) return;

  /* Seek to the end of the file, and read the pointer to the
     beginning of the tag table. */
  if (lseek (z->ifd, -PACK_SIZE, SEEK_END) < 0)
    error ("seeking in input file");

  tag_pointer = read_int_from_zip (z);

  /* Seek to the beginning of the tag table, and try to verify
     the magic string. */
  if (lseek (z->ifd, tag_pointer, SEEK_SET) < 0)
    error ("seeking in input file");

  if (read (z->ifd, magic_buf, ZTAG_MAGIC_LENGTH) != ZTAG_MAGIC_LENGTH) 
    error ("reading input file");

  if (strncmp (magic_buf, ZTAG_MAGIC, ZTAG_MAGIC_LENGTH) != 0) {
    /* At one point, this pointer was off by two longwords... */
    if (read (z->ifd, magic_buf, ZTAG_MAGIC_LENGTH) != ZTAG_MAGIC_LENGTH) 
      error ("reading input file");
    if (read (z->ifd, magic_buf, ZTAG_MAGIC_LENGTH) != ZTAG_MAGIC_LENGTH) 
      error ("reading input file");
    if (strncmp (magic_buf, ZTAG_MAGIC, ZTAG_MAGIC_LENGTH) != 0)
      error ("bad tag table magic number");
  }

  /* Read the table. */
  z->b.nblocks = read_int_from_zip (z);
  z->b.nblocks_allocated = z->b.nblocks;
  z->b.blocks = NALLOC (struct block_record, z->b.nblocks);

  for (i=0; i<z->b.nblocks; i++) {
    z->b.blocks[i].block_start  = read_int_from_zip (z);
    z->b.blocks[i].zblock_start = read_int_from_zip (z);
  }
}


local
struct block_record *find_block (struct zunzip_desc *z, ulg pos)
{
  int lo, hi, mid;
  struct block_record *b;

  if (z->b.blocks == NULL)
    read_block_table (z);

  lo = 0;
  hi = z->b.nblocks;

  while (hi > lo+1) {
    mid = (hi + lo) / 2;
    b = &z->b.blocks[mid];
    if (b->block_start == pos)
      return b;
    else if (b->block_start > pos)
      hi = mid;
    else
      lo = mid;
  }

  return &z->b.blocks[lo];
}


void _zunzip_seek (struct zunzip_desc *z, ulg pos)
{
  struct block_record *b = find_block (z, pos);
  ulg byte_offset;
  int bit_offset;

#if 0
  printf ("seeking %d; found %d %d\n", pos, b->block_start, b->zblock_start);
  {
    int i;
    printf ("block table:\n");
    for (i=0; i<z->b.nblocks; i++)
      printf ("%d %d\n",
	      z->b.blocks[i].block_start,
	      z->b.blocks[i].zblock_start);
  }
#endif

  byte_offset = b->zblock_start>>3;
  bit_offset = b->zblock_start & 7;

  if (lseek (z->ifd, byte_offset, SEEK_SET) < 0)
    error ("seeking in input file");

  /* Clear out anything remaining in the output buffer. */
  z->zoutbuf_ptr = z->zoutbuf;
  z->zoutbuf_len = 0;

  /* And the input buffer. */
  z->insize = z->inptr = 0;

  /* Reset the slide buffer. */
  z->outcnt = 0;
  memset (z->slide, 0, WSIZE);

  /* Compute how much to skip. */
  z->bytes_to_skip = pos - b->block_start;

  inflate_skip_bits (z, bit_offset);
}


/* Send a block of uncompressed data to the output `file'.
   Called by inflate when it has filled up its sliding window.
   We copy the data into a private buffer, from which we then try to
   fulfil Zebra read requests.

   This buffer is pointed to by zoutbuf, and is zoutbuf_size chars long.
   zoutbuf_ptr points to the next char to be copied to Zebra,
   and zoutbuf_len is the length of the data in the buffer remaining
   to be copied to Zebra. */

void flush_window (struct zunzip_desc *z)
{
  if (z->outcnt == 0) return;   /* Nothing to do. */

  /* If adding the block to the buffer would cause us to fall off the end,
     reset zoutbuf_ptr to the beginning of the buffer and shift down
     the remaining contents. */
  if (z->zoutbuf_ptr + z->zoutbuf_len + z->outcnt >
        z->zoutbuf + z->zoutbuf_size) {
    memcpy (z->zoutbuf, z->zoutbuf_ptr, z->zoutbuf_len);
    z->zoutbuf_ptr = z->zoutbuf;

    /* Abort if there's really not enough space. */
    if (z->zoutbuf_len + z->outcnt > z->zoutbuf_size)
      error ("zoutbuf overflow");
  }

  /* Add the data to our buffer. */
  memcpy (z->zoutbuf_ptr + z->zoutbuf_len, z->slide, z->outcnt);
  z->zoutbuf_len += z->outcnt;

  /* Clear the inflate output buffer. */
  z->outcnt = 0;
}


/* Called by inflate to get more data from the input file. */

int fill_inbuf (struct zunzip_desc *z, int eof_ok)
{
  int len;

  /* Read as much as possible */
  z->insize = 0;
  do {
    int len;
    len = read (z->ifd, z->inbuf + z->insize, INBUFSIZ - z->insize);
    if (len == 0 || len == EOF) break;
    z->insize += len;
  } while (z->insize < INBUFSIZ);

  if (z->insize == 0) {
    if (eof_ok) return EOF;
    perror (z->fname);
    error ("reading input");
  }

  z->inptr = 1;
  return z->inbuf[0];
}


/* Open the file FNAME for reading compressed input.
   Returns a pointer to a descriptor which must be provided
   to the other zunzip routines.  Returns NULL if the input file
   couldn't be opened. */

struct zunzip_desc *_zunzip_open (char *fname)
{
  int fd;
  struct zunzip_desc *z;

  /* Open the input file. */
  fd = open (fname, O_RDONLY | O_BINARY, RW_USER,
	     "mbf=3", "mbc=127", "rop=RAH");
  if (fd == -1)
    return NULL;

  /* Allocate the descriptor and store the file descriptor and filename. */
  z = XALLOC (struct zunzip_desc);
  z->ifd = fd;
  z->fname = NALLOC (char, strlen (fname)+1);
  strcpy (z->fname, fname);

  /* Allocate working space needed by inflate. */
  z->inbuf   = NALLOC (uch, INBUFSIZ + INBUF_EXTRA);
  z->zoutbuf = NALLOC (char, ZOUTBUF_SIZE);
  z->slide   = NALLOC (uch, WSIZE);

  /* Initialize. */
  z->outcnt = 0;
  z->insize = z->inptr = 0;
  z->zoutbuf_size = ZOUTBUF_SIZE;
  z->zoutbuf_ptr = z->zoutbuf;
  z->zoutbuf_len = 0;
  z->zeofile = 0;
  z->bytes_to_skip = 0;
  memset (z->slide, 0, WSIZE);

  z->b.nblocks = z->b.nblocks_allocated = 0;
  z->b.blocks = NULL;

  inflate_init (z);

  return z;
}


/* Try to unzip SIZE bytes into the output buffer. */

local
int unzip_bytes (struct zunzip_desc *z, ulg size)
{
  int r, this_size;

  /* Decompress data until we have enough to satisfy the request. */
  while (z->zoutbuf_len < size && !z->zeofile) {
    if ((r = inflate_block (z, &z->zeofile)) != 0) {
      char errbuf[256];
      sprintf (errbuf, "inflate_block error %d", r);
      error (errbuf);
    }
    if (z->zeofile) inflate_finish (z);
  }

  this_size = size;
  if (this_size > z->zoutbuf_len) this_size = z->zoutbuf_len;
  return this_size;
}


local
void skip_bytes (struct zunzip_desc *z, ulg nbytes)
{
  ulg this_skip;

  while (nbytes > 0) {
    this_skip = nbytes;
    if (this_skip > ZOUTBUF_SIZE/2) this_skip = ZOUTBUF_SIZE/2;
    this_skip = unzip_bytes (z, this_skip);
    if (this_skip == 0) break;
    z->zoutbuf_ptr += this_skip;
    z->zoutbuf_len -= this_skip;
    nbytes -= this_skip;
  }
}


/* Get an uncompressed block of data.
   Returns the number of bytes actually copied, or 0 at EOF. */

int _zunzip_block (struct zunzip_desc *z, char *buf, int size)
{
  int r, this_size;

  if (z->bytes_to_skip > 0) {
    skip_bytes (z, z->bytes_to_skip);
    z->bytes_to_skip = 0;
  }

  this_size = unzip_bytes (z, size);

  memcpy (buf, z->zoutbuf_ptr, this_size);

  z->zoutbuf_ptr += this_size;
  z->zoutbuf_len -= this_size;

  /* If we couldn't supply all that was asked for, null-pad the rest
     of the caller's buffer. */
  if (size > this_size)
    memset (buf+this_size, 0, size - this_size);

  return this_size;
}


/* Close the input file and free resources. */

void _zunzip_close (struct zunzip_desc *z)
{
  close (z->ifd);
  if (z->b.blocks != NULL) free (z->b.blocks);
  free (z->slide);
  free (z->zoutbuf);
  free (z->inbuf);
  free (z->fname);
  free (z);
}

/* bits.c -- output variable-length bit strings
 * Copyright (C) 1992-1993 Jean-loup Gailly
 * This is free software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License, see the file COPYING.
 *
 * Hacked for zzip 10/93  sss
 */


/*
 *  PURPOSE
 *
 *      Output variable-length bit strings. Compression can be done
 *      to a file or to memory. (The latter is not supported in this version.)
 *
 *  DISCUSSION
 *
 *      The PKZIP "deflate" file format interprets compressed file data
 *      as a sequence of bits.  Multi-bit strings in the file may cross
 *      byte boundaries without restriction.
 *
 *      The first bit of each byte is the low-order bit.
 *
 *      The routines in this file allow a variable-length bit value to
 *      be output right-to-left (useful for literal values). For
 *      left-to-right output (useful for code strings from the tree routines),
 *      the bits must have been reversed first with bi_reverse().
 *
 *      For in-memory compression, the compressed bit stream goes directly
 *      into the requested output buffer. The input data is read in blocks
 *      by the mem_read() function. The buffer is limited to 64K on 16 bit
 *      machines.
 *
 *  INTERFACE
 *
 *      void bi_init (struct zzip_desc *z)
 *          Initialize the bit string routines.
 *
 *      void send_bits (struct zzip_desc *z, int value, int length)
 *          Write out a bit string, taking the source bits right to
 *          left.
 *
 *      int bi_reverse (int value, int length)
 *          Reverse the bits of a bit string, taking the source bits left to
 *          right and emitting them right to left.
 *
 *      void bi_windup (struct zzip_desc *z)
 *          Write out any remaining bits in an incomplete byte.
 *
 *      void copy_block(struct zzip_desc*z,char *buf, unsigned len, int header)
 *          Copy a stored block to the zip file, storing first the length and
 *          its one's complement if requested.
 *
 */

#ifdef VMS
#define ZZIP_INC "d0$c_inc:zzip.h"
#else
#define ZZIP_INC "zzip.h"
#endif
#include ZZIP_INC
/* #include "crypt.h"  sss */

#ifdef DEBUG
#  include <stdio.h>
#endif

#ifdef RCSID
static char rcsid[] = "$Id$";
#endif

/* ===========================================================================
 * Local data used by the "bit string" routines.
 */

/*local file_t zfile; /* output gzip file */

struct bi_info {
  unsigned short bi_buf;
  int bi_valid;
#ifdef DEBUG
  ulg bits_sent;
#endif
};

/*local unsigned short bi_buf;*/
/* Output buffer. bits are inserted starting at the bottom (least significant
 * bits).
 */

#define Buf_size (8 * 2*sizeof(char))
/* Number of bits used within bi_buf. (bi_buf might be implemented on
 * more than 16 bits on some systems.)
 */

/*local int bi_valid;*/
/* Number of valid bits in bi_buf.  All bits above the last valid bit
 * are always zero.
 */

#ifdef DEBUG
/*  ulg bits_sent;   /* bit length of the compressed data */
#endif

/* ===========================================================================
 * Initialize the bit string routines.
 */
void bi_init (struct zzip_desc *z)
/*    file_t zipfile; /* output zip file, NO_FILE for in-memory compression */
{
    z->bi = XALLOC (struct bi_info);
    z->bi->bi_buf = 0;
    z->bi->bi_valid = 0;
#ifdef DEBUG
    z->bi->bits_sent = 0L;
#endif
}

/* ===========================================================================
 * Send a value on a given number of bits.
 * IN assertion: length <= 16 and value fits in length bits.
 */
void send_bits(struct zzip_desc *z, int value, int length)
/*    int value;  /* value to send */
/*    int length; /* number of bits */
{
  struct bi_info *bi = z->bi;
#ifdef DEBUG
    Tracev((stderr," l %2d v %4x ", length, value));
    Assert(length > 0 && length <= 15, "invalid length");
    bi->bits_sent += (ulg)length;
#endif
    /* If not enough room in bi_buf, use (valid) bits from bi_buf and
     * (16 - bi_valid) bits from value, leaving (width - (16-bi_valid))
     * unused bits in value.
     */
    if (bi->bi_valid > (int)Buf_size - length) {
        bi->bi_buf |= (value << bi->bi_valid);
        put_short(z, bi->bi_buf);
        bi->bi_buf = (ush)value >> (Buf_size - bi->bi_valid);
        bi->bi_valid += length - Buf_size;
    } else {
        bi->bi_buf |= value << bi->bi_valid;
        bi->bi_valid += length;
    }
}

/* ===========================================================================
 * Reverse the first len bits of a code, using straightforward code (a faster
 * method would use a table)
 * IN assertion: 1 <= len <= 15
 */
unsigned bi_reverse(code, len)
    unsigned code; /* the value to invert */
    int len;       /* its bit length */
{
    register unsigned res = 0;
    do {
        res |= code & 1;
        code >>= 1, res <<= 1;
    } while (--len > 0);
    return res >> 1;
}

/* ===========================================================================
 * Write out any remaining bits in an incomplete byte.
 */
void bi_windup (struct zzip_desc *z)
{
    struct bi_info *bi = z->bi;
    if (bi->bi_valid > 8) {
        put_short(z, bi->bi_buf);
    } else if (bi->bi_valid > 0) {
        put_byte(z, bi->bi_buf);
    }
    bi->bi_buf = 0;
    bi->bi_valid = 0;
#ifdef DEBUG
    bi->bits_sent = (bi->bits_sent+7) & ~7;
#endif
}

/* ===========================================================================
 * Copy a stored block to the zip file, storing first the length and its
 * one's complement if requested.
 */
void copy_block (struct zzip_desc *z, char *buf, unsigned len, int header)
/*    char     *buf;    /* the input data */
/*    unsigned len;     /* its length */
/*    int      header;  /* true if block header must be written */
{
    bi_windup (z);              /* align on byte boundary */

    if (header) {
        put_short(z, (ush)len);   
        put_short(z, (ush)~len);
#ifdef DEBUG
        z->bi->bits_sent += 2*16;
#endif
    }
#ifdef DEBUG
    z->bi->bits_sent += (ulg)len<<3;
#endif
    while (len--) {
#ifdef CRYPT
        int t;
	if (key) zencode(*buf, t);
#endif
	put_byte(z, *buf++);
    }
}


#ifdef DEBUG
int bits_sent (struct zzip_desc *z)
{
  return z->bi->bits_sent;
}
#endif
